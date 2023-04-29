class Person
    def initialize(name, age)
        @name = name
        @age = age
    end 

    def getName()
        @name
    end

    def getAge()
        @age
    end

    def setAge(x)
        @age = x 
    end

end 

class Student < Person
    def initialize(name, age, grade)
        super(name, age)
        @grade = grade
    end

    def getGrade
        @grade
    end

    def changeGrade(x)
        @grade = x
    end
end

class Staff < Person
    def initialize (name, age, position)
        super(name,age)
        @position = position
    end

    def getPosition
        @position 
    end 

    def changePosition(newPosition)
        @position = newPosition
    end 
end 
class Roster
    def initialize
        @roster = []
    end 

    def add(person)
        if person.is_a?(Person) 
            @roster.push(person)
        end 
    end 

    def remove(person)
        if @roster.include?(person)==true
            @roster.pop(person)
        end
    end

    def size
        @roster.length()
    end

    def getPerson(name)
        for person in @roster
            puts person.getName()
            if person.getName().eql?(name)
                return person
            end
        end
    end

    def map
        if block_given?
            for person in @roster
                yield person
            end
        end
    end
end  