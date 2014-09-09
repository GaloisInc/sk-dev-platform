{-# OPTIONS_GHC -Wall #-}
module Support
    ( startProcess
    , inner         -- Inner process for use cases
    ) where

-- import IO ( openFile, Handle, hPutStrLn, hGetLine, IOMode ( .. ) )

startProcess :: String -> String -> IO ()
startProcess prefix n = putStrLn ( prefix ++ "Starting process " ++ n )

-- Switch back to readFile and writeFile
inner :: String -> FilePath -> FilePath -> IO ()
inner n inf outf =
  do { putStrLn ( ">>> starting process: " ++ n )
       -- We are ingoring (for now) the possibility of IO errors
       -- | port in  : { position = object };
       -- | domain inf  = SimpleFile ( X, infName );
     ; fileContents <- readFile inf
       -- | domain outf = SimpleFile ( Y, outfName );
       -- | port out : { position = object };
     ; writeFile outf fileContents
     }

-- Use of readFile and writeFile means to loop necessary
-- ToDo change this to use "forever"
-- Maybe no need for external function anymore.
-- loop :: Handle -> Handle -> IO ()
-- loop inf outf
--   = do { -- | in        -- inf.read;
--          -- | inf.write -- p.active;
--        ; l2 <- hGetLine inf
--          -- | p.active  --> outf.write;
--        ; hPutStrLn outf l2
--          -- | outf.read --> out;
--        ; l3 <- hGetLine outf
--        ; putStrLn l3
--        ; loop inf outf
--        }
