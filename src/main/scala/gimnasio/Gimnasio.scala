package gimnasio
import scala.util.{ Success, Failure, Try }
package object Gimnasio {

  case class Pokemon(estado: Estado = SinEstado, stats: Stats) {

    def aumentarEnergia(cantidad: Int): Pokemon =
      copy(stats = stats.copy(energia = stats.energia + cantidad))

    def aumentarExperiencia(cantidad: Int): Pokemon =
      copy(stats = stats.copy(experiencia = stats.experiencia + cantidad))

    def realizarRutina(actividades: Rutina): Try[Pokemon] =
      actividades.foldLeft[Try[Pokemon]](Success(this)) {
        (pokemon: Try[Pokemon], actividad: Actividad) =>
          pokemon match {
            case Success(poke) => realizarActividad(poke, actividad)
            case Failure(_)    => pokemon
          }
      }

    def realizarActividad(pokemon: Pokemon, actividad: Actividad): Try[Pokemon] = {
      Try(
        actividad(pokemon).estado match {
          case Inconsciente        => throw ElPokemonEstaKO
          case Dormido(veces: Int) => Dormido(veces)(pokemon)
          case _                   => actividad(pokemon)
        })
    }

    def despertarse: Pokemon =
      copy(estado = SinEstado)

    def energiaMaxima: Int =
      stats.especie.energiaMaxima

  }

  trait Especie {
    var energiaMaxima: Int = 100
    var fuerza: Int = 100
    var velocidad: Int = 100
  }
  case object Pikachu extends Especie
  case object Gengar extends Especie

  trait TipoPokemon
  case object Normal extends TipoPokemon
  case object Fantasma extends TipoPokemon
  case object Veneno extends TipoPokemon
  case object Pelea extends TipoPokemon
  case object Agua extends TipoPokemon
  case object Fuego extends TipoPokemon

  trait Actividad {
    def apply(pokemon: Pokemon): Pokemon
  }

  case object Descansar extends Actividad {
    def apply(pokemon: Pokemon): Pokemon =
      pokemon.aumentarEnergia(pokemon.energiaMaxima - pokemon.stats.energia)

  }

  case class LevantarPesas(kg: Int) extends Actividad {
    def apply(pokemon: Pokemon): Pokemon =
      (pokemon.stats.tipoPrincipal, pokemon.stats.tipoSecundario) match {
        case (Pelea, _) | (_, Pelea) => pokemon.aumentarExperiencia(2 * kg)
        case (Fantasma, _)           => throw new Exception("Los pokemon de tipo fantasma no pueden levantar pesas")
        case _                       => pokemon.aumentarExperiencia(kg)
      }
  }

  case class Nadar(minutos: Int) extends Actividad {
    def apply(pokemon: Pokemon): Pokemon =
      pokemon.aumentarEnergia(-minutos);
  }
  
  case class Atacar(ataque : Ataque) extends Actividad {
    def apply(pokemon : Pokemon) : Pokemon = ???
  }
  

  abstract class Ataque(pa: Int, tipo : TipoPokemon, efectoColateral : Option[Pokemon => Pokemon] = None)
  case object Mordida extends Ataque(2, Normal)
  case object Reposar extends Ataque(4, Normal)

  
  type Rutina = List[Actividad]

  trait Estado

  case object SinEstado extends Estado
  case class Dormido(vecesQueEstuvoDormido: Int) extends Estado {
    def apply(pokemon: Pokemon): Pokemon = {
      if (vecesQueEstuvoDormido == 3)
        pokemon.despertarse
      else
        pokemon.copy(estado = Dormido(vecesQueEstuvoDormido = vecesQueEstuvoDormido + 1))
    }
  }
  case object Paralizado extends Estado
  case object Inconsciente extends Estado

  case class Stats(especie: Especie, tipoPrincipal: TipoPokemon, tipoSecundario: TipoPokemon, experiencia: Int, energia: Int)

}