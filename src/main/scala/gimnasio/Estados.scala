package gimnasio

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

