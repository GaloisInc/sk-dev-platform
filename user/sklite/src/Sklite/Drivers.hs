module Sklite.Drivers
    ( generateDrivers
    )
where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import System.FilePath ((</>))
import Text.PrettyPrint
import Numeric (showHex)

import Sklite.Types
import qualified Sklite.Paths as Paths
import Sklite.Layout.Validation

-- This is going to be gross for now.  In the future, we might
-- consider the 'language-c' package for generating C code.

generateDrivers :: FilePath -> ExplodedLayout -> IO ()
generateDrivers destDir (ExplodedLayout (ValidatedLayout layout)) = do
  let f cell = buildDriverSource cell
               (getRegions layout (cellSharedMemoryRegions cell))
               (chans cell)
      chans cell = [ c | c <- layoutChannels layout
                   , cellName cell `elem` [chanFrom c, chanTo c]
                   ]
      cfiles = concat $ f <$> layoutCells layout
  writeSourceFiles destDir cfiles

writeSourceFiles :: FilePath -> [(FilePath, String)] -> IO ()
writeSourceFiles destDir files =
    forM_ files $ \(filename, contents) ->
        writeFile (destDir </> filename) (contents ++ "\n")

getRegions :: Layout
           -> [SharedMemoryAccess]
           -> [(SharedMemoryAccess, SharedMemoryRegion)]
getRegions _ [] = []
getRegions layout (a:as) =
    ((,) <$> (pure a) <*> thisRegion) ++ rest
        where
          thisRegion = filter (\r -> accessRegionName a == regionName r)
                       (sharedMemoryRegions layout)
          rest = getRegions layout as

buildDriverSource :: Cell
                  -> [(SharedMemoryAccess, SharedMemoryRegion)]
                  -> [Channel]
                  -> [(FilePath, String)]
buildDriverSource cell regions chans =
    [ (Paths.driverCfilename cell, render $ driver_c cell regions chans)
    , (Paths.driverHfilename cell, render $ driver_h cell regions chans)
    ]

regionBasename :: SharedMemoryAccess -> String
regionBasename a = "region_" ++ (accessAlias a)

regionBaseAddr :: SharedMemoryAccess -> String
regionBaseAddr a = regionBasename a ++ "_base"

regionBasePtr :: SharedMemoryAccess -> String
regionBasePtr a = regionBasename a ++ "_base_ptr"

readable :: SharedMemoryAccess -> Bool
readable a = accessType a `elem` [MemReadOnly, MemReadWrite]

writable :: SharedMemoryAccess -> Bool
writable a = accessType a `elem` [MemWriteOnly, MemReadWrite]

driver_h :: Cell
         -> [(SharedMemoryAccess, SharedMemoryRegion)]
         -> [Channel]
         -> Doc
driver_h cell regions chans =
    vcat [ text "#ifndef __DRIVER_H__"
         , text "#define __DRIVER_H__"
         , text "#include <sys/types.h>"
         , text "#include <stddef.h>"
         , text "#include <string.h>"
         , text "#include <mem.h>"
         , text "#include <channels.h>"
         , vcat $ prototypes <$> regions
         , vcat $ chanVar <$> chans
         , text "int cell_main(int argc, char **argv);"
         , text "#endif"
         ]
        where
          chanVar ch = text "extern" <+>
                       (if chanTo ch == cellName cell then text "read" else text "write") <>
                       text "_channel_p" <+> (text $ chanName ch) <> text ";"

          prototypes (access, region) =
              let required =
                      [ "void * " ++ regionBaseAddr access ++ "(void);"
                      , "#define " ++ regionBasename access ++ "_size() "
                                       ++ (show $ regionSize region)
                      , "struct mem " ++ regionBasename access ++ "_mem();"
                      ]
                  optional = [ if writable access
                               then Just $ "#define " ++ regionBasename access ++ "_write(src, sz, offset) { memcpy("
                                        ++ regionBaseAddr access ++ "() + offset, src, sz); }"
                               else Nothing
                             , if readable access
                               then Just $ "#define " ++ regionBasename access ++ "_read(dst, sz, offset) { memcpy(dst, "
                                        ++ regionBaseAddr access ++ "() + offset, sz); }"
                               else Nothing
                             ]

              in vcat $ text <$> (required ++ catMaybes optional)

driver_c :: Cell
         -> [(SharedMemoryAccess, SharedMemoryRegion)]
         -> [Channel]
         -> Doc
driver_c cell regions chans =
    vcat [ text "#include <sys/types.h>"
         , text "#include <sys/stat.h>"
         , text "#include <sys/mman.h>"
         , text "#include <pwd.h>"
         , text "#include <stdio.h>"
         , text "#include <errno.h>"
         , text "#include <fcntl.h>"
         , text "#include <fcntl.h>"
         , text "#include <string.h>"
         , text "#include <unistd.h>"
         , text "#include <stdlib.h>"
         , text "#include <grp.h>"
         , text "#include <mem.h>"
         , text "#include <channels.h>"
         , text "#include <signal.h>"
         , text $ "#include \"" ++ (Paths.driverHfilename cell) ++ "\""
         , vcat $ chanVar <$> chans
         , vcat $ baseAddrVar <$> regions
         , vcat $ baseAddrFunc <$> regions
         , vcat $ memFunc <$> regions
         , vcat [ text "void setup_shmem() {"
                , nest 4 $ setupShmemBody regions
                , text "}"
                ]
         , vcat [ text "void setup_channels() {"
                , nest 4 $ setupChannelsBody cell chans
                , text "}"
                ]
         , if cellRunMethod cell == CellMain
           then text ""
           else vcat [ text "static void segv_handler(int signal, siginfo_t *si, void *arg) {"
                     , text "  switch (signal) {"
                     , text "  case SIGBUS:"
                     , text "    fprintf(stderr, \"Bus error at %p\\n\", si->si_addr);"
                     , text "    break;"
                     , text "  case SIGSEGV:"
                     , text "    fprintf(stderr, \"Segmentation fault at %p\\n\", si->si_addr);"
                     , text "    break;"
                     , text "  default:"
                     , text "    fprintf(stderr, \"Unexpected signal: %d\\n\", signal);"
                     , text "    break;"
                     , text "  }"
                     , text "  exit(EXIT_FAILURE);"
                     , text "}"
                     , text ""
                     , text "static void setup_segv_handler(void) {"
                     , text ""
                     , text "  int res;"
                     , text "  struct sigaction sa ="
                     , text "    { .sa_sigaction = segv_handler"
                     , text "    , .sa_flags = SA_SIGINFO"
                     , text "    };"
                     , text "  sigemptyset(&sa.sa_mask);"
                     , text ""
                     , text "  res = sigaction(SIGSEGV, &sa, NULL);"
                     , text "  res = sigaction(SIGBUS, &sa, NULL);"
                     , text "  if (res == -1) {"
                     , text "    perror(\"sigaction\");"
                     , text "    exit(EXIT_FAILURE);"
                     , text "  }"
                     , text "}"
                     ]
         , vcat [ text "int main(int argc, char **argv) {"
                , nest 4 $ mainBody cell
                , text "}"
                ]
         ]
        where
          chanVar ch = (if chanTo ch == cellName cell then text "read" else text "write") <>
                       text "_channel_p" <+> (text $ chanName ch) <+> equals <+> text "NULL;"

          memFunc (access, _) =
              vcat [ text $ "struct mem " ++ regionBasename access ++ "_mem() {"
                   , nest 4 $ vcat [ text "struct mem m;"
                                   , text $ "m.base = " ++ regionBaseAddr access ++ "();"
                                   , text $ "m.size = " ++ regionBasename access ++ "_size();"
                                   , text "return m;"
                                   ]
                   , text "}"
                   ]
          baseAddrVar (access, _) =
              text $ concat [ "void * "
                            , regionBasePtr access
                            , " = NULL;"
                            ]
          baseAddrFunc (access, _) =
              vcat [ text $ concat [ "void * "
                                   , regionBaseAddr access
                                   , "() {"
                                   ]
                   , nest 4 $ text $ concat [ "return "
                                            , regionBasePtr access
                                            , ";"
                                            ]
                   , text "}"
                   ]

mainBody :: Cell -> Doc
mainBody cell =
    vcat $ [ text "struct passwd *user_entry = NULL;"
           , text $ "const char *username = " ++ (show $ cellUser cell) ++ ";"
           , text "setup_shmem();"
           , text "setup_channels();"
           -- Reset errno because getpwnam will not do it.
           , text "errno = 0;"
           , text "user_entry = getpwnam(username);"
           , text "if (user_entry == NULL) {"
           , nest 4 $ vcat [ text "printf(\"Error getting user information for '%s': %s\", username, strerror(errno));"
                           , text "return 1;"
                           ]
           , text "}"
           , text "setuid(user_entry->pw_uid);"
           , text "setgid(user_entry->pw_gid);"
           , text "setgroups(0, NULL);"
           ] ++ runProgram
    where
        runProgram = case cellRunMethod cell of
                       CellMain -> runCellMain
                       RawBinary addr p sz -> runFromAddr addr p sz
        runCellMain = [ text "return cell_main(argc, argv);"
                      ]
        runFromAddr addr p sz = [ text $ "int prog = open(" ++ show p ++ ", O_RDWR);"
                                , text $ "struct stat s;"
                                , text "if (0 != fstat(prog, &s)) {"
                                , nest 4 $ vcat [ text "printf(\"Error in fstat(): %s\\n\", strerror(errno));"
                                                , text "return 1;"
                                                ]
                                , text "}"
                                , text $ "void *entry = (void *) " ++ show addr ++ ";"
                                , text $ "void *result = mmap(entry, " ++ show sz ++ ", "
                                  ++ "PROT_READ|PROT_WRITE|PROT_EXEC, MAP_ANONYMOUS|MAP_PRIVATE, -1, 0);"
                                , text $ "if (result != entry) {"
                                , nest 4 $ vcat [ text "printf(\"Error mapping entry point address: %s\\n\", strerror(errno));"
                                                , text "printf(\"Got entry = %p, result = %p\\n\", entry, result);"
                                                , text "return 1;"
                                                ]
                                , text "}"
                                , text "if (s.st_size != read(prog, entry, s.st_size)) { exit(1); }"
                                , text $ "if (mprotect(entry, " ++ show sz ++ ", PROT_READ|PROT_WRITE|PROT_EXEC)) {"
                                , nest 4 $ vcat [ text "printf(\"Error calling mprotect: %s\\n\", strerror(errno));"
                                                , text "return 1;"
                                                ]
                                , text "}"
                                , text "setup_segv_handler();"
                                , text "asm(\"jmpq *%0\"::\"r\"(entry):\"%rax\");"
                                , text "return 0;"
                                ]

setupChannelsBody :: Cell
                  -> [Channel]
                  -> Doc
setupChannelsBody cell chans =
    let chanFunc ch = if cellName cell == chanFrom ch
                      then text "write_channel"
                      else text "read_channel"
        args ch = [ text "region_" <> (text $ slotsBufName ch) <> text "_mem()"
                  , text "region_" <> (text $ readerBufName ch) <> text "_mem()"
                  , text "region_" <> (text $ writerBufName ch) <> text "_mem()"
                  ]
        f ch = let allArgs = args ch ++ [ integer $ chanMsgSize ch
                                        , integer $ chanMsgSlots ch
                                        ] ++
                             if cellName cell == chanFrom ch
                             then [int $ if chanOverwrite ch then 1 else 0]
                             else []
               in [ (text $ chanName ch) <+> equals <+> (chanFunc ch) <>
                    (parens $ hcat $ punctuate comma allArgs) <>
                    semi
                  , text "if" <+> (parens $ ((text $ chanName ch) <+> text "== NULL")) <+> lbrace
                  , nest 4 $ vcat [ text "printf(\"Error setting up channel\\n\");"
                                  , text "exit(1);"
                                  ]
                  , rbrace
                  ]

    in vcat $ concat $ f <$> chans

setupShmemBody :: [(SharedMemoryAccess, SharedMemoryRegion)]
               -> Doc
setupShmemBody regions =
    vcat $ uncurry setupShmemBody_ <$> zip [0..] regions

setupShmemBody_ :: Int -> (SharedMemoryAccess, SharedMemoryRegion) -> Doc
setupShmemBody_ pos (access, region) =
    let (prot, openFlags) = case accessType access of
                              MemReadOnly -> ("PROT_READ", "O_RDONLY")
                              -- NOTE: for write-only cases we still
                              -- open the file read-write, because
                              -- mmap() will refuse to create
                              -- MAP_SHARED mappings if the file is
                              -- not opened this way.  We still rely
                              -- on static types to prevent the user
                              -- from actually writing to the memory
                              -- as long as they use the included
                              -- APIs.
                              MemWriteOnly -> ("PROT_WRITE", "O_RDWR")
                              MemReadWrite -> ("PROT_READ|PROT_WRITE", "O_RDWR")

        fdVar = "fd" ++ show pos
        mapAddr = case accessMapAddress access of
                    Just a -> "(void *) 0x" ++ showHex a ""
                    Nothing -> "NULL"

    in vcat [ text $ "int " ++ fdVar ++ " = open("
              ++ (show $ Paths.regionFilename region)
              ++ ", " ++ openFlags ++ ");"
            , text $ "if (" ++ fdVar ++ " == -1) {"
            , nest 4 $ vcat [ text $ "printf(\"Error opening shared memory file %s: %s\\n\", "
                              ++ (show $ Paths.regionFilename region)
                              ++ ", strerror(errno));"
                            , text "exit(1);"
                            ]
            , text "}"
            , text $ regionBasePtr access ++ " = "
              ++ "mmap(" ++ mapAddr ++ ", " ++ regionBasename access ++ "_size(), "
              ++ prot ++ ", MAP_SHARED, " ++ fdVar ++ ", 0);"
            , text $ "if (" ++ regionBasePtr access ++ " == MAP_FAILED || (" ++ mapAddr ++
              " != NULL && " ++ regionBasePtr access ++ " != " ++ mapAddr ++ ")) {"
            , nest 4 $ vcat [ text "printf(\"Error mapping shared memory: %s\\n\", strerror(errno));"
                            , text "exit(1);"
                            ]
            , text "}"
            ]
