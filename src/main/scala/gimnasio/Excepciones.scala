package gimnasio

case object ElPokemonEstaKOException extends RuntimeException("El pokemon está KO!")

case object PokemonFantasmaNoPuedeTirarPesasException extends RuntimeException("Los pokemon de tipo fantasma no pueden levantar pesas")