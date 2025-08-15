data Person = Person Int Int;

personAge :: Person -> Int;
personAge p = match p { Person name age -> age; };

personName :: Person -> Int;
personName p = match p { Person name age -> name; };

makePerson :: Int -> Int -> Person;
makePerson name age = Person name age;

updatePersonAge :: Person -> Int -> Person;
updatePersonAge p newAge = match p { 
  Person name oldAge -> Person name newAge; 
};

testPerson :: Person;
testPerson = makePerson 100 30;

testAge :: Int;
testAge = personAge testPerson;

testName :: Int;
testName = personName testPerson;

testUpdated :: Person;
testUpdated = updatePersonAge testPerson 31;