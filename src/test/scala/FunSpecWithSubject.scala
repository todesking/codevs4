package com.todesking.codevs4.runner

import org.scalatest.FunSpec

trait FunSpecWithSubject extends FunSpec {
  import org.scalatest.{Tag, Status, Args}

  val subjectStore = new ThreadLocal[Any]

  var currentSubject: () => Any = { () => null }

  class SubjectAccess[A] {
    def apply(): A =
      subjectStore.get.asInstanceOf[A]
  }

  def with_subject[A](subject: => A)(fun: SubjectAccess[A] => Unit): Unit = {
    val oldSubject = currentSubject
    currentSubject = { () => subject }
    fun(new SubjectAccess[A])
    currentSubject = oldSubject
  }

  class ItWordWithSubject extends ItWord {
    override def apply(specText: String, testTags: Tag*)(testFun: => Unit) {
      val subject = currentSubject
      super.apply(specText, testTags:_*) {
        subjectStore.set(subject())
        testFun
      }
    }
  }

  protected override val it = new ItWordWithSubject

}
