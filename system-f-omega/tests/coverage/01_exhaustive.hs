data Maybe a = Nothing | Just a;

test :: Maybe Int -> Int;
test m = match m {
    Nothing -> 42;
    Just x -> x;
};
