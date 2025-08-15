-- Test lambda expressions (using function definition syntax for now)

double :: Int -> Int;
double x = x * 2;

testLambda :: Int;
testLambda = double 21;