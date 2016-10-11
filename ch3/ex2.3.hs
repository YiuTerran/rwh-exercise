avg :: Fractional a => [a] -> Maybe a
avg [] = Nothing
avg x = Just $ (sum x) / (fromIntegral $ length x)