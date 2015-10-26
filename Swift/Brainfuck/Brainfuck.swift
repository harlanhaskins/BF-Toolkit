enum Operator {
    case ModifyPointer(amount: Int32)
    case ModifyValue(amount: Int)
    case Loop(ops: [Operator])
    case Input
    case Output
    case Clear
    
    var compiledDescription: String {
        switch self {
        case .ModifyPointer(let amount):
            return Array(count: Int(abs(amount)), repeatedValue: (amount < 0 ? "<" : ">")).joinWithSeparator("")
        case .ModifyValue(let amount):
            return Array(count: Int(abs(amount)), repeatedValue: (amount < 0 ? "-" : "+")).joinWithSeparator("")
        case .Loop(let ops):
            return "[" + ops.map { $0.compiledDescription }.joinWithSeparator("") + "]"
        case .Input: return ","
        case .Output: return "."
        case .Clear: return "<clr>" // "[-]"
        }
    }
}

extension Operator: Equatable {}
func ==(lhs: Operator, rhs: Operator) -> Bool {
    switch (lhs, rhs) {
    case (.ModifyPointer(let amount), .ModifyPointer(let amount2)) where amount == amount2: return true
    case (.ModifyValue(let amount), .ModifyValue(let amount2)) where amount == amount2: return true
    case (.Loop(let ops), .Loop(let ops2)) where ops == ops2: return true
    case (.Input, .Input): return true
    case (.Output, .Output): return true
    case (.Clear, .Clear): return true
    default: return false
    }
}

enum BrainfuckError: ErrorType {
    case ImbalancedLoops(index: Int)
    case InvalidCharacter
    case OutOfBounds
}

struct Brainfuck {
    let instructions: [Operator]
    let input: () -> Character
    let output: Character -> ()
    var memorySpace = Array<UInt8>(count: 30000, repeatedValue: 0)
    var p = 0
    
    init(instructions: [Operator], input: () -> Character, output: Character -> ()) {
        self.instructions = instructions
        self.input = input
        self.output = output
    }
    
    init(program: String, optimized: Bool = false, input: () -> Character, output: Character -> ()) throws {
        self.instructions = try Brainfuck.compile(program, optimized: optimized)
        self.input = input
        self.output = output
    }

    static func compile(input: String, optimized: Bool) throws -> [Operator] {
        var program = [Operator]()
        var stack = [Operator]()
        for char in input.characters {
            let add = { (op: Operator) in
                if case .Loop(var values)? = stack.last {
                    values.append(op)
                    stack[stack.count - 1] = .Loop(ops: values)
                } else {
                    program.append(op)
                }
            }
            let pc = program.count
            switch "\(char)" {
            case "[": stack.append(.Loop(ops: []))
            case "]":
                guard let loop = stack.last else {
                    throw BrainfuckError.ImbalancedLoops(index: pc)
                }
                stack = Array(stack.dropLast())
                add(loop)
            case "+": add(.ModifyValue(amount: 1))
            case "-": add(.ModifyValue(amount: -1))
            case ">": add(.ModifyPointer(amount: 1))
            case "<": add(.ModifyPointer(amount: -1))
            case ",": add(.Input)
            case ".": add(.Output)
            default: break
            }
        }
        guard stack.isEmpty else {
            throw BrainfuckError.ImbalancedLoops(index: program.count)
        }
        return optimized ? optimize(program) : program
    }
    
    static func numberOfInstructions(program: [Operator]) -> Int {
        var count = 0
        for op in program {
            count++
            if case .Loop(let ops) = op {
                count += Brainfuck.numberOfInstructions(ops)
            }
        }
        return count
    }
    
    static func optimize(program: [Operator]) -> [Operator] {
        guard !program.isEmpty else { return program }
        var newProgram = [Operator]()
        for op in program {
            
            switch op {
            case .Loop(let ops):
                if case .ModifyValue(_)? = ops.first where ops.count == 1 {
                    newProgram.append(.Clear)
                }  else {
                    newProgram.append(.Loop(ops: optimize(ops)))
                }
                continue
            default: break
            }
            
            guard let last = newProgram.last else {
                newProgram.append(op)
                continue
            }
            
            switch (op, last) {
            case (.ModifyPointer(let value), .ModifyPointer(let value2)):
                newProgram = Array(newProgram.dropLast())
                newProgram.append(.ModifyPointer(amount: value + value2))
            case (.ModifyValue(let value), .ModifyValue(let value2)):
                newProgram = Array(newProgram.dropLast())
                newProgram.append(.ModifyValue(amount: value + value2))
            default:
                newProgram.append(op)
            }
        }
        
        return newProgram.filter { op in
            switch op {
            case .ModifyPointer(let amount) where amount == 0: return false
            case .ModifyValue(let amount) where amount == 0: return false
            case .Loop(let ops) where ops.isEmpty: return false
            default: return true
            }
        }
    }
    
    mutating func run() throws {
        try run(self.instructions)
    }
    
    mutating private func run(program: [Operator]) throws {
        for instruction in program {
            switch instruction {
            case .ModifyValue(let amount):
                if amount < 0 {
                    memorySpace[p] = memorySpace[p] &- UInt8(abs(amount))
                } else {
                    memorySpace[p] = memorySpace[p] &+ UInt8(amount)
                }
            case .ModifyPointer(let amount):
                p += Int(amount)
            case .Output:
                output(Character(UnicodeScalar(memorySpace[p])))
            case .Input:
                let inputChar = input()
                if let int = String(inputChar).unicodeScalars.first?.value where Int32(int) != EOF {
                    memorySpace[p] = UInt8(int)
                }
            case .Loop(let ops):
                while memorySpace[p] != 0 {
                    try run(ops)
                }
            case .Clear:
                memorySpace[p] = 0
            }
        }
    }
}