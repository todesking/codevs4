package com.todesking.codevs4.runner

import org.scalatest.FunSpec

trait RSpecLikeSpec extends FunSpec {
  import org.scalatest.{Tag, Status, Args}

  var subjectStack = Seq.empty[SubjectAccess[_]]

  class SubjectAccess[A](val generate: () => A) {
    val store = new ThreadLocal[Any]

    def reload(): Unit =
      store.set(generate())

    def challange(n: Int)(fun: Int => Unit): Unit = {
      (1 to n).foreach { i =>
        reload()
        fun(i)
      }
    }

    def apply(): A =
      store.get.asInstanceOf[A]
  }

  def withSubject[A](subject: => A)(fun: SubjectAccess[A] => Unit): Unit = {
    val generator = {() => subject}
    val access = new SubjectAccess[A](generator)
    val oldStack = subjectStack
    subjectStack :+= access
    fun(access)
    subjectStack = oldStack
  }

  class ItWordWithSubject extends ItWord {
    override def apply(specText: String, testTags: Tag*)(testFun: => Unit) {
      val stack = subjectStack
      super.apply(specText, testTags:_*) {
        stack.foreach { access =>
          access.reload()
        }
        testFun
      }
    }
  }

  protected override val it = new ItWordWithSubject

  protected def describeSubject[A](description: String, newSubject: => A)(fun: SubjectAccess[A] => Unit): Unit = {
    withSubject(newSubject) { subject =>
      describe(description) {
        fun(subject)
      }
    }
  }
}
