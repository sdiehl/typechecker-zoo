data Person = Person String Int;

personName :: Person -> String;
personName p = match p { Person name age -> name; };

personAge :: Person -> Int;
personAge p = match p { Person name age -> age; };

makePerson :: String -> Int -> Person;
makePerson name age = Person name age;

testPerson :: Person;
testPerson = makePerson "Alice" 30;

testName :: String;
testName = personName testPerson;