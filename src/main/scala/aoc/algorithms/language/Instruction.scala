package aoc.algorithms.language

trait Instruction[T](val name: String, val arity: Int)

trait InstructionCall[T, A](val instruction: Instruction[T], val args: Vector[A], execution: Vector[A] => T):
    def execute: T = execution(args)

case class GenericInstruction[T](override val name: String, override val arity: Int) extends Instruction[T](name, arity)

case class GenericInstructionCall[T, A](override val instruction: Instruction[T], override val args: Vector[A], execution: Vector[A] => T) extends InstructionCall[T, A](instruction, args, execution)

object Instruction:
    def apply[T](name: String, arity: Int): Instruction[T] = GenericInstruction(name, arity)

object InstructionCall:
    def apply[T, A](instruction: Instruction[T], args: Vector[A], execution: Vector[A] => T): InstructionCall[T, A] = GenericInstructionCall(instruction, args, execution)