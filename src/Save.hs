module Save where

import Type

save :: State -> IO()
save s = writeFile "save/testwrite.txt" (show s)
