-- Record like functionality using Algebraic Data Types
-- This is actually more idiomatic in functional languages

-- Traditional record like data types
data Person = Person String Int;  -- Person name age
data Point = Point Int Int;       -- Point x y  
data Rectangle = Rectangle Point Point; -- Rectangle topLeft bottomRight

-- Accessor functions (getters)
personName :: Person -> String;
personName p = match p { Person name age -> name; };

personAge :: Person -> Int;
personAge p = match p { Person name age -> age; };

pointX :: Point -> Int;
pointX p = match p { Point x y -> x; };

pointY :: Point -> Int; 
pointY p = match p { Point x y -> y; };

-- Constructor functions (smart constructors)
makePerson :: String -> Int -> Person;
makePerson name age = Person name age;

makePoint :: Int -> Int -> Point;
makePoint x y = Point x y;

-- Update functions (since data is immutable)
updatePersonAge :: Person -> Int -> Person;
updatePersonAge p newAge = match p { 
  Person name oldAge -> Person name newAge; 
};

updatePointX :: Point -> Int -> Point;
updatePointX p newX = match p {
  Point oldX y -> Point newX y;
};

-- More complex record like structures
data Employee = Employee Person String Int; -- Employee person department salary

employeePerson :: Employee -> Person;
employeePerson emp = match emp { Employee person dept salary -> person; };

employeeDepartment :: Employee -> String;
employeeDepartment emp = match emp { Employee person dept salary -> dept; };

employeeSalary :: Employee -> Int;
employeeSalary emp = match emp { Employee person dept salary -> salary; };

-- Nested access (composition)
employeeName :: Employee -> String;
employeeName emp = personName (employeePerson emp);

employeeAge :: Employee -> Int;
employeeAge emp = personAge (employeePerson emp);

-- Test data
testPerson :: Person;
testPerson = makePerson "Alice" 30;

testPoint :: Point;
testPoint = makePoint 10 20;

testEmployee :: Employee;
testEmployee = Employee testPerson "Engineering" 75000;

-- Usage demonstrations
testPersonName :: String;
testPersonName = personName testPerson;

testEmployeeName :: String;
testEmployeeName = employeeName testEmployee;

testUpdatedPerson :: Person;
testUpdatedPerson = updatePersonAge testPerson 31;

-- Complex operations
calculateArea :: Rectangle -> Int;
calculateArea rect = match rect {
  Rectangle topLeft bottomRight -> 
    multiply 
      (pointX bottomRight - pointX topLeft)
      (pointY bottomRight - pointY topLeft);
};

testRectangle :: Rectangle;
testRectangle = Rectangle (makePoint 0 0) (makePoint 10 5);

testArea :: Int;
testArea = calculateArea testRectangle;