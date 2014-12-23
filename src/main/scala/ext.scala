package com.todesking.codevs4.runner

object Ext {
  implicit class ArrayBufferExt[A](self: scala.collection.mutable.ArrayBuffer[A]) {
    def removeIf(pred: A => Boolean): Unit = {
      var i = 0
      var j = 0
      while(i < self.size) {
        if(pred(self(i))) {
          i += 1
        } else {
          if(i > j)
            self(j) = self(i)
          i += 1
          j += 1
        }
      }
      if(i > j)
        self.remove(j, i - j)
    }
  }
}
