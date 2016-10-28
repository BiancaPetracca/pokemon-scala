package gimnasio
import scala.util.{ Success, Failure, Try }
import gimnasio.Gimnasio._
import org.scalatest.FreeSpec
import org.scalatest.Matchers
import org.scalatest.BeforeAndAfter

import org.scalatest.Matchers
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatchResult

class GimnasioTest extends FreeSpec with Matchers with BeforeAndAfter {

  var gengar = Pokemon(stats = new Stats(Gengar, Fantasma, Veneno, 100, 0))
  val actividadesConPesas = List(LevantarPesas(10), Descansar)
  val actividadesParaFantasmas = List(Descansar, Descansar)

  "Gimnasio" - {

    "Actividades" - {

      "Al hacer que gengar realice actividades" in {

        gengar.realizarRutina(actividadesParaFantasmas).get.stats.energia should be(100)
      }

      "Al estar dormido tres veces se despierta" in {
        gengar = gengar.copy(estado = Dormido(0))
        val actividades = List(Descansar, Nadar(10), Nadar(3), Nadar(1))
        gengar.realizarRutina(actividades).get.estado should be(SinEstado)
      }

      "Al intentar levantar pesas no puede" in {
        intercept[Exception] {
          gengar.realizarRutina(actividadesConPesas).get

        }
      }

    }
  }
}