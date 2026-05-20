data Either a b = Left a | Right b;
data Maybe a = Nothing | Just a;

test :: Maybe (Either Int Int) -> Int;
test m = match m {
    Nothing -> 0;
    Just (Left x) -> x;
};
