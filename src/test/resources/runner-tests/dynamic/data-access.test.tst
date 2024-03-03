import io

def main() = {
    var fido = Dog("Fido")
    print(fido.name)
}

data Dog {
    name: Str
}

// EXPECTED-OUTPUT
// Fido
