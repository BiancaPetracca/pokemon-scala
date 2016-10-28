package gimnasio

import scala.util.{ Success, Failure, Try }

case class Pokemon(estado: Estado = SinEstado, stats: Stats) {

  type Rutina = List[Actividad]

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
        case Inconsciente        => throw ElPokemonEstaKOException
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

case class Stats(especie: Especie, tipoPrincipal: TipoPokemon, tipoSecundario: TipoPokemon, experiencia: Int, energia: Int)
