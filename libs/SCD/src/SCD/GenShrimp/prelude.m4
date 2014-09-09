define(`Allow',`allow $1 $2;
')
define(`SubjIfcs',`ifelse($2,,,`Allow($2,$1)'`SubjIfcs($1,shift(shift($@)))')')
define(`ObjIfcs',`ifelse($2,,,`SubjIfcs($2,$1)'`ObjIfcs(`$1',shift(shift($@)))')')
define(`Connect',`ObjIfcs(`$1',$2)')
