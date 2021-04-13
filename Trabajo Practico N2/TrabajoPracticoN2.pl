
/* ********************
* Modelo del Dominio *
******************** */

% Punto 1
%region(region,zona).
region(eriador, laComarca).
region(eriador, rivendel).
region(montaniasNubladas, moria).
region(montaniasNubladas, lothlorien).
region(rohan, edoras).
region(rohan, isengard).
region(rohan, abismoDeHelm).
region(gondor, minasTirith).
region(mordor, minasMorgul).
region(mordor, monteDelDestino).


%Punto 1.a

cantidadDeRegiones(ListaDeZonas, CantDeRegiones):- 
    findall(Region, (member(Zona,ListaDeZonas), region(Region, Zona)), Regiones),
    list_to_set(Regiones, PasaPorRegion),
    length(PasaPorRegion, CantDeRegiones).

%Punto 1.b

esVueltero(Zonas):-
    length(Zonas, Cantidad1),
    sort(Zonas, Lista),
    length(Lista, Cantidad2),
    Cantidad2<Cantidad1.

%Punto 1.c

todosLosCaminosConducenAMordor(Caminos):- 
    forall(ultimaZona(Caminos, Zona), seEncuentraEnMordor(Zona)).

seEncuentraEnMordor(Zona):-
    region(mordor, Zona).

ultimaZona(Caminos, UltimaZona):-
    nth0(_, Caminos, Camino),
    last(Camino, UltimaZona).

%Punto 2.

%viajero(nombre, raza(nivel, poderMagico))
viajero(gandalf, maiar(25, 260)).

%Punto 2.b

%viajero(nombre, tipoDePersonaje( raza, [armas y niveles])).
viajero(legolas, guerrero(elfo, [arma(espada, 20), arma(arco, 29)])).
viajero(gimli, guerrero(enano, [arma(hacha, 26)])).
viajero(aragorn, guerrero(dunedian, [arma(espada, 30)])).
viajero(gorbag, guerrero(orco, [arma(ballesta, 24)])).
viajero(boromir, guerrero(humano,[arma(ballesta, 26)])).
viajero(ugluk, guerrero(urukHai, [arma(espada, 22), arma(arco, 26)])).
%Punto 2.c

%viajero(nombre, tipoDePersonaje(raza, edad)).
viajero(frodo, pacifista(hobbit,51)).
viajero(sam, pacifista(hobbit, 36)).
viajero(barbol, pacifista(ent, 5300)).

%Punto 3.a

razaViajero(Viajero, Raza):-
   viajero(Viajero,TipoDeViajero),
   devolverRaza(TipoDeViajero, Raza).

devolverRaza(guerrero(Raza, _), Raza).
devolverRaza(pacifista(Raza, _), Raza).
devolverRaza(maiar(_, _), maiar).

%Punto 3.b 

tieneArmas(Viajero, Arma):-
    viajero(Viajero, TipoDePersonaje),
    devolverArma(TipoDePersonaje, Arma).

devolverArma(pacifista(ent,_), fuerza).

devolverArma(pacifista(hobbit, Anios), daga):-
    Anios =< 50.

devolverArma(pacifista(hobbit, Anios), espadaCorta):-
    Anios > 50.

devolverArma(maiar(_,_), baston).

devolverArma(guerrero(_, Armas), Arma):-
    darArma(Armas, Arma).
darArma(Armas, Arma):-
    nth0(_, Armas, arma(Arma, _)).
    
%Punto 3.c

nivelDelViajero(Viajero,Nivel):-
    viajero(Viajero, maiar(Nivel,_)).

nivelDelViajero(Viajero,Nivel):-
    viajero(Viajero, pacifista(hobbit, Edad)),
    Nivel is Edad/4.

nivelDelViajero(Viajero,Nivel):-
    viajero(Viajero, pacifista(ent, Edad)),
    Nivel is Edad/100.

nivelDelViajero(Viajero, Nivel):-
    viajero(Viajero, TipoDePersonaje),
    listaDeArmas(TipoDePersonaje, Armas),
    mayorNivelArma(Armas, Nivel).

listaDeArmas(guerrero(_, Armas), Armas).

mayorNivelArma(Armas, NivelMax):-
    findall(Nivel,(member(Arma, Armas),darNivel(Nivel, Arma)), ListaDeNiveles),
    max_member(NivelMax, ListaDeNiveles).

darNivel(Nivel, arma(_, Nivel)).

%Punto 4.a

grupo(ListaDeViajeros):-
    findall(Viajero, viajero(Viajero,_), Viajeros),
    combinacion(Viajeros, ListaDeViajeros),
    ListaDeViajeros  \= [].

combinacion(_, []). 
combinacion(Lista1, [X|Lista2]):-
    select(X,Lista1,Lista3), 
    combinacion(Lista3, Lista2).   

%Punto 4.b

zonaRequerimiento(minasTirith, integrante(maiar, 25)).
zonaRequerimiento(moria, elemento(armaduraMithril, 1)).
zonaRequerimiento(isengard, integrante(maiar, 27)).
zonaRequerimiento(isengard, magia(280)).
zonaRequerimiento(abismoDeHelm, integrante(elfo, 28)).
zonaRequerimiento(abismoDeHelm, integrante(enano, 20)).
zonaRequerimiento(abismoDeHelm, integrante(maiar, 25)).
zonaRequerimiento(abismoDeHelm, magia(200)).
zonaRequerimiento(sagrario, elemento(anduril, 1)).
zonaRequerimiento(minasMorgul, elemento(lembas, 2)).
zonaRequerimiento(minasMorgul, elemento(luzEarendil, 1)).

tiene(sam, lembas).
tiene(sam, lembas).
tiene(sam, lembas).
tiene(gandalf, sombraGris).
tiene(frodo, armaduraMithril).
tiene(frodo, luzEarendil).
tiene(frodo, lembas).
tiene(frodo, capaElfica).
tiene(sam, capaElfica).
tiene(legolas, capaElfica).
tiene(aragorn, capaElfica).
tiene(aragorn, anduril).

cumpleRequerimiento(Viajeros, Requerimiento):-
    grupo(Viajeros),
    validacionDeRequerimiento(Viajeros, Requerimiento).

magiaDelViajero(Viajero, PoderMagico):-
    viajero(Viajero, maiar(_,PoderMagico)).

magiaDelViajero(Viajero, PoderMagico):-
    razaViajero(Viajero,Raza),
    member(Raza, [humano, ent, hobbit, urukHai]),
    PoderMagico is 0.

magiaDelViajero(Viajero, PoderMagico):-
    nivelDelViajero(Viajero, Nivel),
    razaViajero(Viajero, Raza),
    poderMagico(Raza,Nivel, PoderMagico).

poderMagico(elfo, Nivel, PoderMagico):-        
    PoderMagico is Nivel*2.

poderMagico(Raza, Nivel, PoderMagico):-
    member(Raza, [enano,dunedian]),
    PoderMagico is Nivel.

siEstaPoneUno(ListaDeElementos, ElementoR,  Cantidad):-
    member(ElementoR, ListaDeElementos),
    Cantidad is 1.

elementosDeLosViajeros(Viajeros, Elementos):-
    findall(ElementoDelViajero,(member(Viajero, Viajeros), tiene(Viajero, ElementoDelViajero)),Elementos).

validacionDeRequerimiento(Viajeros, magia(NivelMinimo)):-
    findall(Magia,(member(Viajero, Viajeros), magiaDelViajero(Viajero, Magia)), Niveles),
    sum_list(Niveles, NivelTotal),
    NivelTotal >= NivelMinimo.
 
validacionDeRequerimiento(Viajeros, elemento(ElementoDeRequerimiento, CantidadMinima)):-
    elementosDeLosViajeros(Viajeros, Elementos),
    findall(Tiene,siEstaPoneUno(Elementos, ElementoDeRequerimiento, Tiene),Tienen),
    sum_list(Tienen, CantidadDeElementosTotal),
    CantidadDeElementosTotal >= CantidadMinima.

validacionDeRequerimiento(Viajeros, integrante(RazaDeRequerimiento,NivelMinimo)):-
    razaViajero(Viajero, RazaDeRequerimiento),
    member(Viajero, Viajeros),
    nivelDelViajero(Viajero, NivelViajero),
    NivelViajero >= NivelMinimo.

%Punto 5.a

puedeAtravesar(Zona, Viajeros):-
    grupo(Viajeros),
    region(_, Zona),
    forall(zonaRequerimiento(Zona, Requerimiento), cumpleRequerimiento(Viajeros, Requerimiento)).

%Punto 5.b

seSienteComoEnCasa(Viajeros, Region):-
    grupo(Viajeros),
    region(Region,_),
    forall(region(Region, Zona), puedeAtravesar(Zona, Viajeros)).

/*-----------------------------------------
-------------------Test-------------------
-----------------------------------------*/

%Punto 1.a

:-begin_tests(cantidadDeRegionesTest).

test(un_camino_que_pasa_por_la_comarca_rivendel_y_moria_atraviesa_2_regiones, nondet):-
    cantidadDeRegiones([laComarca, rivendel, moria], 2).

test(camino_que_pasa_por_minasTirith_y_minasMorgul_atraviesa_2_regiones, set(Cantidad == [2])):-
    cantidadDeRegiones([minasTirith, minasMorgul], Cantidad).

test(el_camino_moria_sengard_edoras_minas_morgul_minasTirith_no_atraviesa_2_regiones, fail):-
    cantidadDeRegiones([moria, isengard, edoras, minasMorgul, minasTirith], 2).

:- end_tests(cantidadDeRegionesTest).


%Punto 1.b

:-begin_tests(noCaminoVueltero).

test(el_camino_no_es_vueltero, fail):-
    esVueltero([rivendel, laComarca]).

:-end_tests(noCaminoVueltero).

:-begin_tests(esCaminoVueltero).

test(el_camino_es_vueltero):-
    esVueltero([rivendel, laComarca, rivendel]).

:-end_tests(esCaminoVueltero).

%Punto 1.c

:- begin_tests(caminosQueTerminanEnMordor).
test(camino_que_conduce_a_mordor):-
    todosLosCaminosConducenAMordor([[minasTirith, minasMorgul],[minasTirith, minasMorgul, monteDelDestino], [monteDelDestino, minasMorgul, minasTirith, minasMorgul]]).

:- end_tests(caminosQueTerminanEnMordor).

:- begin_tests(caminosQueNoTerminanEnMordor).
test(caminos_que_no_terminan_en_mordor,fail):-
    todosLosCaminosConducenAMordor([[minasTirith, minasMorgul], [minasTirith, minasMorgul, monteDelDestino], [monteDelDestino, minasMorgul, minasTirith]]).
:- end_tests(caminosQueNoTerminanEnMordor).

%Punto 3.a

:- begin_tests(razaViajeroTest).

test(gandalf_es_un_maiar):-
    razaViajero(gandalf, maiar).

test(legolas_es_un_elfo):-
    razaViajero(legolas, elfo).

test(gorbag_es_un_orco):-
    razaViajero(gorbag, orco).

test(ugluk_es_un_urukHai):-
    razaViajero(ugluk, urukHai).

test(frodo_es_un_hobbit):-
    razaViajero(frodo, hobbit).

test(barbol_es_un_ent):-
    razaViajero(barbol, ent).

:- end_tests(razaViajeroTest).

%Punto 3.b

:- begin_tests(tieneBaston).
test(gandalf_tiene_un_baston, set(Viajero == [gandalf])):-
    tieneArmas(Viajero, baston).
:- end_tests(tieneBaston).

:- begin_tests(usaEspadaCorta).
test(frodo_tiene_usa_espada_corta, nondet):-
    tieneArmas(frodo, espadaCorta).
:-end_tests(usaEspadaCorta).

:- begin_tests(usaDaga).
test(sam_tiene_usa_daga, nondet):-
    tieneArmas(sam, daga).
:-end_tests(usaDaga).

% Punto 3.c

:-begin_tests(nivel_que_tienen_los_viajeros).
test(nivel_de_gandalf, nondet):-
    nivelDelViajero(gandalf, 25).

test(nivel_de_gorbag, set(Nivel == [24])):-
    nivelDelViajero(gorbag, Nivel).

test(nivel_de_legolas):-
    nivelDelViajero(legolas, 29).

test(nivel_de_sam, nondet):-
    nivelDelViajero(sam, 9).

test(nivel_de_barbol, set(Nivel == [53]) ):-
    nivelDelViajero(barbol, Nivel).

test(quien_tiene_nivel_26, set(Quien == [gimli, boromir, ugluk])):-
    nivelDelViajero(Quien, 26).

test(quien_tiene_nivel_de_26, set(Quien == [])):-
    nivelDelViajero(Quien, 22).

test(existe_alguien_con_25, set(Existe == [gandalf])):-
    nivelDelViajero(Existe, 25).

:-end_tests(nivel_que_tienen_los_viajeros).

%Punto 4.a

:- begin_tests(grupoValidoTest).

test(un_grupo_formado_por_frodo_sam_merry_y_pippin_no_es_un_grupo_valido, fail):-
    grupo([frodo, sam, merry, pippin]).

test(un_grupo_formado_por_legolas_y_gimli_es_un_grupo_valido, nondet):-
    grupo([legolas, gimli]).

:- end_tests(grupoValidoTest).


ponerEnUnaLista(X, Y, [X, Y]).

:- begin_tests(existen90Soluciones).
test(comprobar_que_existen_90_soluciones_en_grupos_de_2_viajeros, set(CantSoluciones == [90])):-
    findall([X, Y], (grupo([X, Y]), ponerEnUnaLista(X, Y, [X, Y])), Soluciones), length(Soluciones, CantSoluciones).
:- end_tests(existen90Soluciones).


%Punto 4.b

:-begin_tests(pasa_por_abismo_de_helm).

test(gandalf_aragorn_legolas_gimli_cumplen_el_requerimiento_de_magia_de_nivel_200, nondet):-
    cumpleRequerimiento([gandalf, aragorn, gimli], magia(200)).

:-end_tests(pasa_por_abismo_de_helm).

:-begin_tests(no_pasa_por_isengard).

test(gandalf_aragorn_y_gimli_no_cumplen_con_integrante_de_raza_maiar_de_nivel_27, fail):-
    cumpleRequerimiento([gandalf, aragorn, gimli], integrante(maiar, 27)).

:-end_tests(no_pasa_por_isengard).

:-begin_tests(pasa_por_moria).
test(gandalf_aragorn_y_frodo ,nondet):-
    cumpleRequerimiento([gandalf,aragorn,frodo], elemento(armaduraMithril, 1)).

test(gandalf_legolas_y_aragorn_cumplen_el_requerimiento_integrante_de_draza_maiar_de_nivel_25, nondet):-
    cumpleRequerimiento([gandalf, legolas, aragorn], integrante(maiar,25)).

test(gorbag_y_ugluk_no_cumplen_el_requerimiento_integrante_de_raza_maiar_nivel_25, fail):-
    cumpleRequerimiento([gorbag, ugluk],integrante(maiar,25)).
:-end_tests(pasa_por_moria).

:-begin_tests(pasa_por_minas_morgul).
test(frodo_y_sam_cumple_con_el_requerimiento_morgul, nondet):-
    cumpleRequerimiento([frodo, sam], elemento(lembas, 2)).
:-end_tests(pasa_por_minas_morgul).


%Punto 5.a
:-begin_tests(puedePasarPorUnaZona).
test(gandalf_aragorn_y_frodo_puede_pasar_por_moria,nondet):-
    puedeAtravesar(moria, [gandalf, aragorn, frodo]).

test(existen_grupos_que_pueden_pasar_por_Isengard, fail):-
 puedeAtravesar(isengard, [X, Y]).

test(un_grupo_formado_por_frodo_y_sam_puede_pasar_por_minas_morgul, nondet):-
    puedeAtravesar(minasMorgul, [frodo, sam]).
end_tests(puedePasarPorUnaZona).

%Punto 5.b

:-begin_tests(no_sienten_como_en_casa).

test(frodo_y_sam_no_se_sienten_como_en_casa, fail):-
    seSienteComoEnCasa([frodo,sam], rohan).

test(existe_un_grupo_de_viajeros_que_se_siente_como_en_casa, fail):-
    seSienteComoEnCasa([X, Y], rohan).

:-end_tests(no_sienten_como_en_casa).

:-begin_tests(siente_como_en_casa).
test(frodo_sam_y_se_siente_como_en_casa_en_eriador, nondet):-
    seSienteComoEnCasa([frodo, sam], eriador).
:-end_tests(siente_como_en_casa).