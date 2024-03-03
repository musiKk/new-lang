import io

def main() = {
    var fido = Dog("Fido")
    fido.bark()
    Dog.bark(fido)
    print(fido.info())
    print(Dog.info(fido))
}

data Dog {
    name: Str
}

def Dog.bark() = print("woof")
def Dog.info() = "Dog is named " + this.name

// EXPECTED-OUTPUT
// woof
// woof
// Dog is named Fido
// Dog is named Fido
