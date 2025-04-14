package calico
package html

import cats.effect.IO
import fs2.{Pipe, Stream}
import fs2.concurrent.Signal
import org.scalajs.dom.{document, Event, HTMLElement, HTMLInputElement}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*

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
