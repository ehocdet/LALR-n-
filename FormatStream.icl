
implementation module FormatStream

import StdEnv, StdFile

TS :== 8	// convert all \t by TS ' '

FormatStream :: !File -> [String]
FormatStream f = FS (sfreadline f)
where
	FS (s, f) 
		| i==0            = []
		| s.[i-1] <> '\n' = [TabToSpace 0 (s +++ "\n"):[]]
				  = [TabToSpace 0 s : FS (sfreadline f)]
	where 
		i = size s
		TabToSpace :: !Int !String -> !String
		TabToSpace f s 
			| n==0	 = s%(f,c)
			| c-n==f = createArray (n*TS) ' ' +++ TabToSpace c s
				= s%(f,c-n)
					+++ createArray (n*TS) ' '
					+++ TabToSpace c s
		where
			(c,n) = searchtab f s
			searchtab c s=:{[c]=e}
				| e=='\n' = (c,0)
				| e=='\t' = searchtab` (c+1) s 1
					= searchtab (c+1) s
			searchtab` c s=:{[c]=e} n
				| e=='\t' = searchtab` (c+1) s (n+1)
					= (c,n)

