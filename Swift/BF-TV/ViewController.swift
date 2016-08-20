//
//  ViewController.swift
//  BF-TV
//
//  Created by Harlan Haskins on 10/20/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

import UIKit

class ViewController: UIViewController {

    @IBOutlet weak var inputTextView: UITextView!
    @IBOutlet weak var outputTextView: UITextView!
    
    var brainfuck: Brainfuck?
    
    override func viewDidLoad() {
        super.viewDidLoad()
        inputTextView.font = UIFont.preferredFont(forTextStyle: .title1)
        outputTextView.font = UIFont.preferredFont(forTextStyle: .title1)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }

    @IBAction func addCharacter(_ sender: UIButton) {
        inputTextView.text = inputTextView.text + (sender.title(for: sender.state) ?? "")
    }
    
    func createBrainfuck() {
        var program = inputTextView.text ?? ""
        var input = ""
        do {
            brainfuck = try Brainfuck(program: program, optimized: true, input: {
                defer {
                    if !input.isEmpty {
                        input = input.substring(from: input.index(input.startIndex, offsetBy: 1))
                    }
                }
                return input.characters.first ?? Character(UnicodeScalar(0))
                }, output: { char in
                    DispatchQueue.main.async {
                        self.outputTextView.text = self.outputTextView.text + "\(char)"
                    }
            })
        } catch {
            outputTextView.text = "\(error)"
        }
    }
    
    @IBAction func runBrainfuck(_ sender: AnyObject) {
        outputTextView.text = ""
        createBrainfuck()
        DispatchQueue.global(qos: DispatchQoS.QoSClass.background).async {
            do {
                try self.brainfuck?.run()
            } catch {
                self.outputTextView.text = "\(error)"
            }
        }
    }
    
}

