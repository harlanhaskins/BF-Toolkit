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
    
    @IBAction func clearInput(_ sender: AnyObject) {
        inputTextView.text = ""
    }
    
    @IBAction func clearOutput(_ sender: AnyObject) {
        outputTextView.text = ""
        output = ""
    }
    
    @IBAction func clearSubInput(_ sender: AnyObject) {
        subInputTextField.text = ""
    }
    
    func createBrainfuck() {
        var program = inputTextView.text ?? ""
        var input = subInputTextField.text ?? ""
        do {
            brainfuck = try Brainfuck(program: program, optimized: true, input: {
                defer {
                    if !input.isEmpty {
                        input = input.substring(from: input.index(input.startIndex, offsetBy: 1))
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
        runButton.isEnabled = false
        compileButton.isEnabled = false
        output = ""
    }
    
    func stopRunning() {
        activityIndicator.stopAnimating()
        runButton.isEnabled = true
        compileButton.isEnabled = true
        self.outputTextView.text = self.output
    }
    
    @IBAction func runBrainfuck(_ sender: AnyObject) {
        outputTextView.text = ""
        createBrainfuck()
        startRunning()
        DispatchQueue.global(qos: DispatchQoS.QoSClass.background).async {
            do {
                try self.brainfuck?.run()
            } catch {
                self.showError(error)
            }
            DispatchQueue.main.async(execute: self.stopRunning)
        }
    }
    
    func showError(_ error: Error) {
        DispatchQueue.main.async {
            self.stopRunning()
            let b = Banner(title: "Error", subtitle: "\(error)", image: nil, backgroundColor: UIColor.red)
            b.preferredStatusBarStyle = .lightContent
            b.adjustsStatusBarStyle = true
            b.show(duration: 3.0)
        }
    }
    
    @IBAction func compileBrainfuckButtonPressed(_ sender: AnyObject) {
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

