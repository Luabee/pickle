# pickle
Instant StatsD in Haskell. 

# Usage
Call `withPickleDo defaultSettings` with your main function, e.g.
```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Map.Strict as M

main :: IO()
main = withPickleDo defaultSettings $ do
{...}
```
Then to send metrics you can just call one of these functions:
```haskell
let name  = "gauge.name"
    value = 42
    tags  = M.fromList [("key","value), ("tag2","value")]
gauge name value (Just tags)
timer name value (Just tags)
counter name value (Just tags)
```

Pickle will take care of the rest. He's a good boy.
![my dogger, pickle](https://i.imgur.com/9WxMnIj.jpg)
