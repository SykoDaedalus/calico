/*
 * Copyright 2022 Arman Bilge
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package calico
package html

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import fs2.Pipe
import fs2.Stream
import fs2.concurrent.Signal
import org.scalajs.dom.Event
import org.scalajs.dom.HTMLElement
import org.scalajs.dom.HTMLInputElement
import org.scalajs.dom.document

object CustomComponent {

  sealed trait HtmlElementType
  class ccinputElem extends HtmlElementType
  class ccdivElem extends HtmlElementType
  class cclabelElem extends HtmlElementType
  class cculistElem extends HtmlElementType
  class ccsectionElem extends HtmlElementType

  trait Setter[E <: HtmlElementType, A, V]:
    def set(elem: E, value: V): IO[Unit]

  trait Emits[E <: HtmlElementType, Ev]:
    def listen(elem: E, pipe: Pipe[IO, Ev, Unit]): IO[Unit]

  class Tag[E <: HtmlElementType](val name: String):
    def create: IO[E] = IO(document.createElement(name).asInstanceOf[E])
    def apply(modifiers: (E => IO[Unit])*): IO[E] =
      for {
        elem <- create
        _ <- modifiers.toList.traverse(_(elem))
      } yield elem

  class Attribute[E <: HtmlElementType, V](val key: String):
    def :=(value: V)(using setter: Setter[E, String, V]): E => IO[Unit] =
      (elem: E) => setter.set(elem, value)

    def <--(signal: Signal[IO, V])(using setter: Setter[E, String, V]): E => IO[Unit] =
      (elem: E) => signal.discrete.evalMap(setter.set(elem, _)).compile.drain

    def -->(listener: Pipe[IO, Event, Unit])(using emits: Emits[E, Event]): E => IO[Unit] =
      (elem: E) => emits.listen(elem, listener)

  given Setter[ccinputElem, String, String] with
    def set(elem: ccinputElem, value: String): IO[Unit] =
      IO(elem.asInstanceOf[HTMLElement].setAttribute("id", value))

  given Setter[ccdivElem, String, String] with
    def set(elem: ccdivElem, value: String): IO[Unit] =
      IO(elem.asInstanceOf[HTMLElement].setAttribute("class", value))

  given Setter[ccinputElem, String, Boolean] with
    def set(elem: ccinputElem, value: Boolean): IO[Unit] =
      IO(elem.asInstanceOf[HTMLInputElement].checked = value)

  given Emits[ccinputElem, Event] with
    def listen(elem: ccinputElem, pipe: Pipe[IO, Event, Unit]): IO[Unit] =
      IO(
        elem
          .asInstanceOf[HTMLElement]
          .addEventListener(
            "input",
            e => pipe(Stream.emit(e).covary[IO]).compile.drain.unsafeRunAndForget()
          )
      )

  given Emits[ccdivElem, Event] with
    def listen(elem: ccdivElem, pipe: Pipe[IO, Event, Unit]): IO[Unit] =
      IO(
        elem
          .asInstanceOf[HTMLElement]
          .addEventListener(
            "click",
            e => pipe(Stream.emit(e).covary[IO]).compile.drain.unsafeRunAndForget()
          )
      )

  val ccinputTag = Tag[ccinputElem]("input")
  val ccdivTag = Tag[ccdivElem]("div")
  val cculTag = Tag[cculistElem]("ul")
  val ccsection = Tag[ccsectionElem]("section")
  val cclabelTag = Tag[cclabelElem]("label")

  val idAttr = Attribute[ccinputElem, String]("id")
  val classAttr = Attribute[ccdivElem, String]("class")
  val typeAttr = Attribute[ccinputElem, String]("type")
  val checkedAttr = Attribute[ccinputElem, Boolean]("checked")
  val onInputAttr = Attribute[ccinputElem, Event]("input")
  val onClickAttr = Attribute[ccdivElem, Event]("click")

}
