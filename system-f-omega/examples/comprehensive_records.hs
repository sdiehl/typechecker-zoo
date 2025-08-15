-- Comprehensive record functionality using ADTs

-- Basic record-like types
data Person = Person Int Int;    -- Person nameId age
data Point = Point Int Int;      -- Point x y  
data Rectangle = Rectangle Point Point; -- Rectangle topLeft bottomRight

-- Single field accessors (getters)
personNameId :: Person -> Int;
personNameId p = match p { Person nameId age -> nameId; };

personAge :: Person -> Int;
personAge p = match p { Person nameId age -> age; };

pointX :: Point -> Int;
pointX p = match p { Point x y -> x; };

pointY :: Point -> Int; 
pointY p = match p { Point x y -> y; };

-- Constructor functions (smart constructors)
makePerson :: Int -> Int -> Person;
makePerson nameId age = Person nameId age;

makePoint :: Int -> Int -> Point;
makePoint x y = Point x y;

makeRectangle :: Point -> Point -> Rectangle;
makeRectangle topLeft bottomRight = Rectangle topLeft bottomRight;

-- Update functions (immutable updates)
updatePersonAge :: Person -> Int -> Person;
updatePersonAge p newAge = match p { 
  Person nameId oldAge -> Person nameId newAge; 
};

updatePointX :: Point -> Int -> Point;
updatePointX p newX = match p {
  Point oldX y -> Point newX y;
};

-- Complex nested record operations
rectangleWidth :: Rectangle -> Int;
rectangleWidth rect = match rect {
  Rectangle topLeft bottomRight -> 
    pointX bottomRight - pointX topLeft;
};

rectangleHeight :: Rectangle -> Int;
rectangleHeight rect = match rect {
  Rectangle topLeft bottomRight -> 
    pointY bottomRight - pointY topLeft;
};

-- Helper function for multiplication
multiply :: Int -> Int -> Int;
multiply x y = x * y;

rectangleArea :: Rectangle -> Int;
rectangleArea rect = 
  multiply (rectangleWidth rect) (rectangleHeight rect);

-- Test data and operations
testPerson :: Person;
testPerson = makePerson 1001 25;

testPoint :: Point;
testPoint = makePoint 10 20;

testRect :: Rectangle;
testRect = makeRectangle (makePoint 0 0) (makePoint 10 5);

-- Usage examples
testPersonNameId :: Int;
testPersonNameId = personNameId testPerson;

testPersonAge :: Int;
testPersonAge = personAge testPerson;

testUpdatedPerson :: Person;
testUpdatedPerson = updatePersonAge testPerson 26;

testUpdatedPersonAge :: Int;
testUpdatedPersonAge = personAge testUpdatedPerson;

testRectArea :: Int;
testRectArea = rectangleArea testRect;

testWidth :: Int;
testWidth = rectangleWidth testRect;

testHeight :: Int;
testHeight = rectangleHeight testRect;