class SomeClass {

};

class OtherClass : public SomeClass {

};

class AnotherClass : public SomeClass, public OtherClass
{

};

class ClassClass :
        public SomeClass,
        public Otherclass
        {

        };