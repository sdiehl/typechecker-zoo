data Maybe a = Nothing | Just a;

test :: Maybe Int -> Int;
test m = match m {
    Nothing -> 0;
    Just x -> x;
    Nothing -> 1;
};
