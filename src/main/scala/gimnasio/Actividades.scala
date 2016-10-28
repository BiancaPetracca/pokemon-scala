package gimnasio

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
      case (Fantasma, _)           => throw PokemonFantasmaNoPuedeTirarPesasException
      case _                       => pokemon.aumentarExperiencia(kg)
    }
}

case class Nadar(minutos: Int) extends Actividad {
  def apply(pokemon: Pokemon): Pokemon =
    pokemon.aumentarEnergia(-minutos);
}

case class Atacar(ataque: Ataque) extends Actividad {
  def apply(pokemon: Pokemon): Pokemon = ???
}

abstract class Ataque(pa: Int, tipo: TipoPokemon, efectoColateral: Option[Pokemon => Pokemon] = None)
case object Mordida extends Ataque(2, Normal)
case object Reposar extends Ataque(4, Normal)

