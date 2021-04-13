/*************************
 * TP de la tierra media *
 *************************/
%:- codificacion(utf8).

/* *********************
* Modelo del dominio *
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

%Punto 2

camino1([laComarca, rivendel, moria, lothlorien, edoras, minasTirith, minasMorgul, monteDelDestino]).
camino2([laComarca, rivendel, moria, lothlorien, edoras, minasTirith, minasMorgul]).
camino3([laComarca, rivendel, moria, lothlorien, edoras, minasTirith]).
camino4([moria, isengard, lothlorien, edoras]).

%Punto 3

limitrofes(rivendel, moria).
limitrofes(moria, isengard).
limitrofes(lothlorien, edoras).
limitrofes(edoras, minasTirith).
limitrofes(minasTirith, minasMorgul).

zonasLimitrofes(Zona1, Zona2) :-
    region(Region, Zona1),
    region(Region, Zona2),
    Zona1 \= Zona2.
zonasLimitrofes(Zona1, Zona2) :- limitrofes(Zona1, Zona2).
zonasLimitrofes(Zona1, Zona2) :- limitrofes(Zona2, Zona1).

%Punto 4.a

regionesLimitrofes(Region1, Region2) :-
    region(Region1, Zona1),
    region(Region2, Zona2),
    Region1 \= Region2,
    zonasLimitrofes(Zona1, Zona2).

%Punto 4.b

region(mordor).
region(rohan).
region(montaniasNubladas).
region(gondor).
region(eriador).

regionesLejanas(Region1,Region2):-
    region(Region1),
    region(Region2),
    Region1 \= Region2,
    not(existeTerceraRegion(Region1,Region2)),
    not(regionesLimitrofes(Region1,Region2)).

existeTerceraRegion(Region1,Region2):-
    regionesLimitrofes(Region1,Region3),
    regionesLimitrofes(Region3,Region2).

%Punto 5.a

puedeSeguirCon(Zonas, Zona):-
    last(Zonas, UltimaZona),
    zonasLimitrofes(UltimaZona, Zona).

%Punto 5.b

sonConsecutivos(PrimerCaminos, SegundaCamino):-
    nth0(0, SegundaCamino, PrimerZona),
    puedeSeguirCon(PrimerCaminos, PrimerZona).

%Punto 6.a
caminoTieneLogica([]).
caminoTieneLogica([_]).
caminoTieneLogica([UnaZona| Zonas]):-
    nth1(2, [UnaZona| Zonas], SegundaZona),
    zonasLimitrofes(UnaZona, SegundaZona),
    caminoTieneLogica(Zonas).

%Punto 6.b
caminoSeguro([]).
caminoSeguro([_]).
caminoSeguro([_,_]).
caminoSeguro([Zona1, Zona2, Zona3|Zonas]):-
    not(verificarZonasSeguras(Zona1, Zona2, Zona3)),
    caminoSeguro([Zona2, Zona3|Zonas]).

verificarZonasSeguras(Zona1, Zona2, Zona3):-
    region(Region, Zona1),
    region(Region, Zona2),
    region(Region, Zona3).

/*-----------------------------------------
                Test
-------------------------------------------*/


%Punto 3

:- begin_tests(zonasLimitrofesTest).

test(la_comarca_y_el_monte_del_destino_no_son_zonas_limitrofe, fail):-
    zonasLimitrofes(laComarca, monteDelDesierto).

test(la_comarca_y_rivendel_son_zonas_limitrofes, nondet):-
    zonasLimitrofes(laComarca, rivendel).

test(rivendel_y_la_comarca_son_zonas_limitrofes,nondet):-
    zonasLimitrofes(rivendel, laComarca).

test(rivendel_y_moria_son_zonas_limitrofes,nondet):-
    zonasLimitrofes(rivendel, moria).

test(moria_y_rivendel_son_zonas_limitrofes,nondet):-
    zonasLimitrofes(moria, rivendel).

test(que_zonas_son_limitrofes_con_rivendel,
    set(Zonas == [laComarca, moria])):-
    zonasLimitrofes(rivendel, Zonas).

:- end_tests(zonasLimitrofesTest).

% Punto 4.a

:- begin_tests(regionesLimitrofesTest).

test(eriador_y_las_montanias_nubladas_son_regiones_limitrofes, nondet):-
    regionesLimitrofes(eriador, montaniasNubladas).

test(rohan_y_mordor_son_regiones_limitrofes, fail):-
    regionesLimitrofes(rohan, mordor).

test(cuales_son_las_regiones_limitrofes_de_eriado,
    set(Regiones == [montaniasNubladas])):-
    regionesLimitrofes(eriador, Regiones).

test(cuales_son_las_regiones_limitrofes_de_gondor,
    set(Regiones == [rohan, mordor])):-
    regionesLimitrofes(gondor, Regiones).

test(isengard_tiene_regiones_limitrofes, fail):-
    regionesLimitrofes(isengard, _).

test(gondor_tiene_regiones_limitrofes, nondet):-
    regionesLimitrofes(gondor, _).

:- end_tests(regionesLimitrofesTest).

% Punto 4.b

:- begin_tests(regionesLejanas).

test(rohan_y_las_montanias_nubladas_no_son_lejanas, fail):-
    regionesLejanas(rohan, montaniasNubladas).

test(eriador_y_mordor_son_regiones_lejanas, nondet):-
    regionesLejanas(eriador, mordor).

test(eriador_y_rohan_no_son_lejanas, fail):-
    regionesLejanas(eriador, rohan).

test(que_region_es_lejana_de_mordor, set(Region == [montaniasNubladas, eriador])):-    
    regionesLejanas(Region,mordor).

test(eriador_es_una_region_lejana_para_gondor):-
    regionesLejanas(eriador,gondor).
:-end_tests(regionesLejanas).

% Punto 5.a

:- begin_tests(puedeSeguirPorPositivo).

test(puedeSeguir_con_funciona_para_consultas_individuales_por_positivo):-
    puedeSeguirCon([laComarca, rivendel, moria, lothlorien, edoras, minasMorgul], minasTirith).

test(puedeSeguir_consulta_existencial, set(Zona == [isengard, abismoDeHelm, minasTirith, lothlorien])):-
    puedeSeguirCon([moria, isengard, lothlorien, edoras], Zona).

:- end_tests(puedeSeguirPorPositivo).

:- begin_tests(puedeSeguirPorNegativo).

test(puedeSeguir_con_funciona_para_consultas_individuales_por_negativo, fail):-
    puedeSeguirCon([moria, isengard, lothlorien, edoras], moria).

:- end_tests(puedeSeguirPorNegativo).

%Punto 5.b

:-begin_tests(caminosNoConsecutivos).
test(los_caminos_no_son_consecutivos, fail):-

    sonConsecutivos([moria, rivendel, laComarca], [laComarca, rivendel]).

:-end_tests(caminosNoConsecutivos).

:-begin_tests(caminosSonConsecutivos).

test(los_caminos_son_consecutivo, nondet):-

    sonConsecutivos([rivendel],[moria,edoras]).

:-end_tests(caminosSonConsecutivos).

% Test Punto 6.a

:- begin_tests(caminoTieneLogicaTest).

test(camino_dado_en_el_enunciado_tiene_logica, nondet):-
    caminoTieneLogica([laComarca, rivendel, moria, lothlorien, edoras, minasTirith, minasMorgul, monteDelDestino]).

test(el_siguiente_camino_no_tiene_logica, fail):-
    caminoTieneLogica([comarca, rivendel, lothlorien, edoras, montedelDestino]).

:- end_tests(caminoTieneLogicaTest).

%Punto 6.b

:- begin_tests(caminoSeguro).

test(es_un_camino_seguro,nondet):-
     
    caminoSeguro([laComarca, rivendel, moria, lothlorien, edoras, minasTirith, minasMorgul, monteDelDestino]).

:- end_tests(caminoSeguro).

:-begin_tests(caminoNoSeguro).

test(no_es_un_camino_seguro, fail):-
    caminoSeguro([rivendel, lothlorien, edoras, isengard, abismoDeHelm, minasTirith, minasMorgul]).

:-end_tests(caminoNoSeguro).
