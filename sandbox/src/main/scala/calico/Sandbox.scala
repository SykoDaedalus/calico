import calico.*
import calico.html.CustomComponent.{*, given}
import calico.unsafe.given
import calico.syntax.*
import cats.effect.*
import fs2.*
import fs2.concurrent.*
import fs2.dom.*
import org.scalajs.dom.{document, Event, HTMLElement, HTMLInputElement}
import cats.syntax.all.*

val placeholderAttr = Attribute[ccinputElem, String]("placeholder")
val labelTextAttr = Attribute[cclabelElem, String]("textContent")
val textContentAttr = Attribute[ccdivElem, String]("textContent")

given Setter[ccdivElem, String, String] with
  def set(elem: ccdivElem, value: String): IO[Unit] =
    IO {
      elem.asInstanceOf[HTMLElement].textContent = value
      elem.asInstanceOf[HTMLElement].setAttribute("class", "dynamic-content")
    }

given Setter[cclabelElem, String, String] with
  def set(elem: cclabelElem, value: String): IO[Unit] =
    IO(elem.asInstanceOf[HTMLElement].textContent = value)

def appendChild[E1 <: HtmlElementType, E2 <: HtmlElementType](parent: E1, child: E2): IO[Unit] =
  IO {
    parent.asInstanceOf[HTMLElement].appendChild(child.asInstanceOf[HTMLElement])
    ()
  }

def handleNameInputPipe(name: SignallingRef[IO, String]): Pipe[IO, Event, Unit] =
  _.evalMap { event =>
    IO {
      val inputValue = event.target.asInstanceOf[HTMLInputElement].value
      val newValue = if inputValue.isEmpty then "world" else inputValue
      name.set(newValue).unsafeRunAsync(_ => ())
    }
  }

val handleClickPipe: Pipe[IO, Event, Unit] =
  _.evalMap(e =>
    IO(
      println(
        s"Clicked on element with class: ${e.target.asInstanceOf[HTMLElement].className}"
      )
    ))

object Sandbox extends IOWebApp {
  def render: Resource[IO, HtmlDivElement[IO]] =
    SignallingRef[IO].of("world").toResource.flatMap { name =>
      Resource.eval {
        for {
          container <- ccdivTag(
            classAttr := "app-container",
            onClickAttr --> handleClickPipe
          )
          label <- cclabelTag(labelTextAttr := "Your name: ")
          _ <- appendChild(container, label)
          input <- ccinputTag(
            idAttr := "name-input",
            placeholderAttr := "Enter your name here",
            onInputAttr --> handleNameInputPipe(name)
          )
          _ <- appendChild(container, input)
          greetingDiv <- ccdivTag()
          _ <- appendChild(container, greetingDiv)
          _ <- IO {
            name
              .discrete
              .map(value => s"Hello, ${value.toUpperCase}")
              .evalMap(greeting =>
                IO(
                  greetingDiv.asInstanceOf[HTMLElement].textContent = greeting
                ))
              .compile
              .drain
              .unsafeRunAndForget()
          }
        } yield container.asInstanceOf[HtmlDivElement[
          IO
        ]]
      }
    }
}
