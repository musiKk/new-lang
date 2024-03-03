import io

def main() = {
    var fido = Dog("Fido")
    describePet(fido, "Ralph")
    describePet(Tortoise(), "Susan")
}

def describePet(pet: Pet, humanName: Str) = {
    print("this is " + pet.getName())
    pet.makeNoise()
    pet.relateToHuman(humanName)
}

data Dog {
    name: Str
}
data Tortoise {}

impl Tortoise is Pet {
    def makeNoise() = print("...")
    def getName() = "tortoise got no name"
    def relateToHuman(hName: Str) = print("tortoise indifferent to " + hName)
}
impl Dog is Pet {
    def makeNoise() = print("woof")
    def getName() = this.name
    def relateToHuman(hName: Str) = print(hName + " is my best friend!")
}

trait Pet {
    def makeNoise()
    def getName(): Str
    def relateToHuman(hName: Str)
}

// EXPECTED-OUTPUT
// this is Fido
// woof
// Ralph is my best friend!
// this is tortoise got no name
// ...
// tortoise indifferent to Susan
