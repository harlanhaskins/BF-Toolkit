//
//  ViewController.swift
//  Brainfuck
//
//  Created by Harlan Haskins on 10/15/15.
//  Copyright © 2015 Harlan Haskins. All rights reserved.
//

import UIKit

class ViewController: UIViewController {
    @IBOutlet weak var inputTextView: UITextView!

    @IBOutlet weak var outputTextView: UITextView!
    @IBOutlet weak var subInputTextField: UITextField!
    @IBOutlet weak var activityIndicator: UIActivityIndicatorView!
    @IBOutlet weak var runButton: UIButton!
    @IBOutlet weak var compileButton: UIButton!
    
    var brainfuck: Brainfuck?
    var output = ""
    
    @IBAction func clearInput(sender: AnyObject) {
        inputTextView.text = ""
    }
    
    @IBAction func clearOutput(sender: AnyObject) {
        outputTextView.text = ""
        output = ""
    }
    
    @IBAction func clearSubInput(sender: AnyObject) {
        subInputTextField.text = ""
    }
    
    func createBrainfuck() {
        var program = inputTextView.text ?? ""
        var input = subInputTextField.text ?? ""
        do {
            brainfuck = try Brainfuck(program: program, optimized: true, input: {
                defer {
                    if !input.isEmpty {
                        input = input.substringFromIndex(input.startIndex.advancedBy(1))
                    }
                }
                return input.characters.first ?? Character(UnicodeScalar(0))
            }, output: { char in
                self.output.append(char)
            })
        } catch {
            showError(error)
        }
    }
    
    func startRunning() {
        activityIndicator.startAnimating()
        runButton.enabled = false
        compileButton.enabled = false
        output = ""
    }
    
    func stopRunning() {
        activityIndicator.stopAnimating()
        runButton.enabled = true
        compileButton.enabled = true
        self.outputTextView.text = self.output
    }
    
    @IBAction func runBrainfuck(sender: AnyObject) {
        outputTextView.text = ""
        createBrainfuck()
        startRunning()
        dispatch_async(dispatch_get_global_queue(QOS_CLASS_BACKGROUND, 0)) {
            do {
                try self.brainfuck?.run()
            } catch {
                self.showError(error)
            }
            dispatch_async(dispatch_get_main_queue(), self.stopRunning)
        }
    }
    
    func showError(error: ErrorType) {
        dispatch_async(dispatch_get_main_queue()) {
            self.stopRunning()
            let b = Banner(title: "Error", subtitle: "\(error)", image: nil, backgroundColor: UIColor.redColor())
            b.preferredStatusBarStyle = .LightContent
            b.adjustsStatusBarStyle = true
            b.show(duration: 3.0)
        }
    }
    
    @IBAction func compileBrainfuckButtonPressed(sender: AnyObject) {
        createBrainfuck()
        guard let bf = brainfuck else {
            return
        }
        outputTextView.text = IREmitter(brainfuck: bf).emit()
    }
    
}

extension UInt8 {
    var charValue: String {
        if Int32(self) == EOF { return "EOF" }
        return "\(Character(UnicodeScalar(self)))"
    }
}

