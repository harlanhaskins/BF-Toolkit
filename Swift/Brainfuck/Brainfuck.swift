enum Operator {
    case modifyPointer(amount: Int32)
    case modifyValue(amount: Int)
    case loop(ops: [Operator])
    case input
    case output
    case clear
    
    var compiledDescription: String {
        switch self {
        case .modifyPointer(let amount):
            return Array(repeating: (amount < 0 ? "<" : ">"), count: Int(abs(amount))).joined(separator: "")
        case .modifyValue(let amount):
            return Array(repeating: (amount < 0 ? "-" : "+"), count: Int(abs(amount))).joined(separator: "")
        case .loop(let ops):
            return "[" + ops.map { $0.compiledDescription }.joined(separator: "") + "]"
        case .input: return ","
        case .output: return "."
        case .clear: return "<clr>" // "[-]"
        }
    }
}

extension Operator: Equatable {}
func ==(lhs: Operator, rhs: Operator) -> Bool {
    switch (lhs, rhs) {
    case (.modifyPointer(let amount), .modifyPointer(let amount2)) where amount == amount2: return true
    case (.modifyValue(let amount), .modifyValue(let amount2)) where amount == amount2: return true
    case (.loop(let ops), .loop(let ops2)) where ops == ops2: return true
    case (.input, .input): return true
    case (.output, .output): return true
    case (.clear, .clear): return true
    default: return false
    }
}

enum BrainfuckError: Error {
    case imbalancedLoops(index: Int)
    case invalidCharacter
    case outOfBounds
}

struct Brainfuck {
    let instructions: [Operator]
    let input: () -> Character
    let output: (Character) -> ()
    var memorySpace = Array<UInt8>(repeating: 0, count: 30000)
    var p = 0
    
    init(instructions: [Operator], input: @escaping () -> Character, output: @escaping (Character) -> ()) {
        self.instructions = instructions
        self.input = input
        self.output = output
    }
    
  init(program: String, optimized: Bool = false, input: @escaping () -> Character, output: @escaping (Character) -> ()) throws {
        self.instructions = try Brainfuck.compile(program, optimized: optimized)
        self.input = input
        self.output = output
    }

    static func compile(_ input: String, optimized: Bool) throws -> [Operator] {
        var program = [Operator]()
        var stack = [Operator]()
        for char in input.characters {
            let add = { (op: Operator) in
                if case .loop(var values)? = stack.last {
                    values.append(op)
                    stack[stack.count - 1] = .loop(ops: values)
                } else {
                    program.append(op)
                }
            }
            let pc = program.count
            switch "\(char)" {
            case "[": stack.append(.loop(ops: []))
            case "]":
                guard let loop = stack.last else {
                    throw BrainfuckError.imbalancedLoops(index: pc)
                }
                stack = Array(stack.dropLast())
                add(loop)
            case "+": add(.modifyValue(amount: 1))
            case "-": add(.modifyValue(amount: -1))
            case ">": add(.modifyPointer(amount: 1))
            case "<": add(.modifyPointer(amount: -1))
            case ",": add(.input)
            case ".": add(.output)
            default: break
            }
        }
        guard stack.isEmpty else {
            throw BrainfuckError.imbalancedLoops(index: program.count)
        }
        return optimized ? optimize(program) : program
    }
    
    static func numberOfInstructions(_ program: [Operator]) -> Int {
        var count = 0
        for op in program {
            count += 1
            if case .loop(let ops) = op {
                count += Brainfuck.numberOfInstructions(ops)
            }
        }
        return count
    }
    
    static func optimize(_ program: [Operator]) -> [Operator] {
        guard !program.isEmpty else { return program }
        var newProgram = [Operator]()
        for op in program {
            
            switch op {
            case .loop(let ops):
                if case .modifyValue(_)? = ops.first, ops.count == 1 {
                    newProgram.append(.clear)
                }  else {
                    newProgram.append(.loop(ops: optimize(ops)))
                }
                continue
            default: break
            }
            
            guard let last = newProgram.last else {
                newProgram.append(op)
                continue
            }
            
            switch (op, last) {
            case (.modifyPointer(let value), .modifyPointer(let value2)):
                newProgram.removeLast()
                newProgram.append(.modifyPointer(amount: value + value2))
            case (.modifyValue(let value), .modifyValue(let value2)):
                newProgram.removeLast()
                newProgram.append(.modifyValue(amount: value + value2))
            default:
                newProgram.append(op)
            }
        }
        
        return newProgram.filter { op in
            switch op {
            case .modifyPointer(let amount) where amount == 0: return false
            case .modifyValue(let amount) where amount == 0: return false
            case .loop(let ops) where ops.isEmpty: return false
            default: return true
            }
        }
    }
    
    mutating func run() throws {
        try run(self.instructions)
    }
    
    mutating private func run(_ program: [Operator]) throws {
        for instruction in program {
            switch instruction {
            case .modifyValue(let amount):
                if amount < 0 {
                    memorySpace[p] = memorySpace[p] &- UInt8(abs(amount))
                } else {
                    memorySpace[p] = memorySpace[p] &+ UInt8(amount)
                }
            case .modifyPointer(let amount):
                p += Int(amount)
            case .output:
                output(Character(UnicodeScalar(memorySpace[p])))
            case .input:
                let inputChar = input()
                if let int = String(inputChar).unicodeScalars.first?.value, Int32(int) != -1 {
                    memorySpace[p] = UInt8(int)
                }
            case .loop(let ops):
                while memorySpace[p] != 0 {
                    try run(ops)
                }
            case .clear:
                memorySpace[p] = 0
            }
        }
    }
}
