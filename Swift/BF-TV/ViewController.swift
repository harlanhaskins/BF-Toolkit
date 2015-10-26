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
        inputTextView.font = UIFont.preferredFontForTextStyle(UIFontTextStyleTitle1)
        outputTextView.font = UIFont.preferredFontForTextStyle(UIFontTextStyleTitle1)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
        // Dispose of any resources that can be recreated.
    }

    @IBAction func addCharacter(sender: UIButton) {
        inputTextView.text = inputTextView.text + (sender.titleForState(sender.state) ?? "")
    }
    
    func createBrainfuck() {
        var program = inputTextView.text ?? ""
        var input = ""
        do {
            brainfuck = try Brainfuck(program: program, optimized: true, input: {
                defer {
                    if !input.isEmpty {
                        input = input.substringFromIndex(input.startIndex.advancedBy(1))
                    }
                }
                return input.characters.first ?? Character(UnicodeScalar(0))
                }, output: { char in
                    dispatch_async(dispatch_get_main_queue()) {
                        self.outputTextView.text = self.outputTextView.text + "\(char)"
                    }
            })
        } catch {
            outputTextView.text = "\(error)"
        }
    }
    
    @IBAction func runBrainfuck(sender: AnyObject) {
        outputTextView.text = ""
        createBrainfuck()
        dispatch_async(dispatch_get_global_queue(QOS_CLASS_BACKGROUND, 0)) {
            do {
                self.brainfuck?.run()
            } catch {
                self.outputTextView.text = "\(error)"
            }
        }
    }
    
}

