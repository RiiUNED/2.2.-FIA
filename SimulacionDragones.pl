% Simulador de ecosistema
% Desarollo de poblaciones de dragones en distintos nucleos solares
% rsanchez628@alumno.uned.es

%Dragones - Constantes
dragon_rojo.
dragon_amarillo.

%fertilidad - Fertilidad(Temperatura, Dragon)
fertilidad(_, dragon_rojo, 2).
fertilidad(_, dragon_amarillo, 2).

%frecuencia - Frecuencia(Temperatura, Dragon)
frecuencia(_, dragon_rojo, 3).
frecuencia(_, dragon_amarillo, 5).

%gula - Gula(Temperatura)
gula(_, 1).

%Estrellas - Constantes
estrella1.
estrella2.
estrella3.

%reset - Reset(Estrella)
reset(estrella1, (11, 1)).
reset(estrella2, (5, 1)).
reset(estrella3, (15, 1)).

%descripcion - Descripcion(Estrella)
descripcion(estrella1, ("Luhman 16", "enana marron", 1210)).
descripcion(estrella2, ("Kraz", "Gigante luminosa", 5100)).
descripcion(estrella3, ("HW Virginis", "Subenana caliente", 30000)).

:- dynamic temp/1. %Predicado dinamico - Auxiliar
temp(_).

:- dynamic inicio/2. %Predicado dinamico - Auxiliar
inicio(_, _).

%Predicado dinamico - BBDD
%Cuenta dragones rojos, amarillos y ciclos.
%Ciclos de extincion de dragones rojos y amarillos.
:- dynamic contador/5.
contador(_, _, _, _, _).

%-------------- Reglas uso general --------------------------------
%Uso del contador
%Establece origen segun estrella - Opcion menu
estado0 :- 
    inicio(Rojo0, Amarillo0),
    Rojo is Rojo0,
    Amarillo is Amarillo0,
    Ciclo is 0,
    Fin_rojo is 0,
    Fin_amarillo is 0,
    retractall(contador(_, _, _, _, _)),
    assert(contador(Rojo, Amarillo, Ciclo, Fin_rojo, Fin_amarillo)).

%Contar Ciclo -> Ciclo = Ciclo + 1.
contar_ciclo :- 
    contador(Rojo, Amarillo, Ciclo, Fin_rojo, Fin_amarillo),
    U is Ciclo + 1,
    retractall(contador(Rojo, Amarillo, _, Fin_rojo, Fin_amarillo)),
    assert(contador(Rojo, Amarillo, U, Fin_rojo, Fin_amarillo)).

%--------------------- Informacion por consola -------------------------
%Informacion por consola del estado del ecosistema
estado:-
    not(extincion_total), not(extincion_rojo), not(extincion_amarillo), estado_pleno;
    not(extincion_total), extincion_rojo, estado_extincion_roja;
    not(extincion_total), extincion_amarillo, estado_extincion_amarilla;
    extincion_total, estado_exticion_total.
%Distintos mensajes segun especie de dragon extinta
estado_pleno :- 
    contador(Rojo, Amarillo, Ciclo, _, _),
    nl,
    write('Dragones ROJOS: '), write(Rojo), nl,
    write('Dragones AMARILLOS: '), write(Amarillo), nl,
    write('Numero de CICLOS: '), write(Ciclo), nl.
estado_exticion_total :-
    contador(_, _, Ciclo, Fin_rojo, Fin_amarillo),
    nl,
    write('Dragones ROJOS EXTINTOS en ciclo: '), write(Fin_rojo), nl,
    write('Dragones AMARILLOS EXTINTOS en ciclo: '), write(Fin_amarillo), nl,
    write('Numero de CICLOS: '), write(Ciclo), nl.
estado_extincion_roja :- 
    contador(_, Amarillo, Ciclo, Fin_rojo, _),
    nl,
    write('Dragones ROJOS EXTINTOS en ciclo: '), write(Fin_rojo), nl,
    write('Dragones AMARILLOS: '), write(Amarillo), nl,
    write('Numero de CICLOS: '), write(Ciclo), nl.
estado_extincion_amarilla :- 
    contador(Rojo, _, Ciclo, _, Fin_amarillo),
    nl,
    write('Dragones ROJOS: '), write(Rojo), nl,
    write('Dragones AMARILLOS EXTINTOS en ciclo: '), write(Fin_amarillo), nl,
    write('Numero de CICLOS: '), write(Ciclo), nl.
%------------------ Fin informacion por consola ----------------------

%------------------ Condicion extincion especies ---------------------
extincion_total :-
    contador(Rojo, Amarillo, Ciclo, Fin_rojo, Fin_amarillo),
    0 is Rojo,
    0 is Fin_rojo,
    0 is Amarillo,
    0 is Fin_amarillo,
    retractall(contador(Rojo, Amarillo, Ciclo, _, _)),
    assert(contador(Rojo, Amarillo, Ciclo, Ciclo, Ciclo));
    contador(Rojo, Amarillo, Ciclo, Fin_rojo, Fin_amarillo),
    0 is Rojo,
    0 is Fin_rojo,
    retractall(contador(Rojo, Amarillo, Ciclo, _, Fin_amarillo)),
    assert(contador(Rojo, Amarillo, Ciclo, Ciclo, Fin_amarillo));
    contador(Rojo, Amarillo, Ciclo, Fin_rojo, Fin_amarillo),
    0 is Amarillo,
    0 is Fin_amarillo,
    retractall(contador(Rojo, Amarillo, Ciclo, Fin_rojo, _)),
    assert(contador(Rojo, Amarillo, Ciclo, Fin_rojo, Ciclo));
    contador(Rojo, Amarillo, _, _, _),
    0 is Rojo,
    0 is Amarillo.
extincion_rojo :-
    contador(Rojo, Amarillo, Ciclo, Fin_rojo, Fin_amarillo),
    0 is Rojo,
    0 is Fin_rojo,
    retractall(contador(Rojo, Amarillo, Ciclo, _, Fin_amarillo)),
    assert(contador(Rojo, Amarillo, Ciclo, Ciclo, Fin_amarillo));
    contador(Rojo, _, _, _, _),
    0 is Rojo.
extincion_amarillo :-
    contador(Rojo, Amarillo, Ciclo, Fin_rojo, Fin_amarillo),
    0 is Amarillo,
    0 is Fin_amarillo,
    retractall(contador(Rojo, Amarillo, Ciclo, Fin_rojo, _)),
    assert(contador(Rojo, Amarillo, Ciclo, Fin_rojo, Ciclo));
    contador(_, Amarillo, _, _, _),
    0 is Amarillo.
%------------------ Fin Condicion extincion especies ---------------------
%-------------- FIN Reglas uso general --------------------------------

%-------------- dragones comiendo------------------------------
%Los dragones amarillos comen dragones rojos si los hay
%En ausencia de dragones rojos, son canibales
comer(Temperatura) :- 
    not(extincion_rojo), amarillo_come_rojo(Temperatura);
    not(extincion_amarillo), amarillo_come_amarillo(Temperatura);
    extincion_rojo, extincion_amarillo.

%El numero de veces que comen los dragones depende de la poblacion de
%la poblacion de cada especie
amarillo_come_amarillo(Temperatura) :-
    contador(_, Amarillo, _, _, _),
    1 is Amarillo,
    comerAA_cantidad(Temperatura, 1);
    contador(_, Amarillo, _, _, _),
    Y is Amarillo // 2,
    comerAA_cantidad(Temperatura, Y).

amarillo_come_rojo(Temperatura) :-
    contador(_, Amarillo, _, _, _),
    comerAR_cantidad(Temperatura, Amarillo).

%Los dragones comidos se descuentan de las poblaciones
comer(Temperatura, dragon_amarillo, Dragon) :- 
    Dragon==dragon_rojo ->
    (
        extincion_rojo;
        descontar(Temperatura, Dragon)
    );
    extincion_amarillo;
    descontar(Temperatura, Dragon).

%descontar(Temperatura, Dragon) -> Dragon = Dragon - 1.
descontar(Temperatura, Dragon) :-
    Dragon==dragon_amarillo ->
    (
        contador(Rojo, Amarillo, Ciclo, Fin_rojo, Fin_amarillo),
        gula(Temperatura, Gula),
        U is Amarillo - Gula,
        retractall(contador(Rojo, _, Ciclo, Fin_rojo, Fin_amarillo)),
        assert(contador(Rojo, U, Ciclo, Fin_rojo, Fin_amarillo))
    );
    contador(Rojo, Amarillo, Ciclo, Fin_rojo, Fin_amarillo),
    gula(Temperatura, Gula),
    U is Rojo - Gula,
    retractall(contador(_, Amarillo, Ciclo, Fin_rojo, Fin_amarillo)),
    assert(contador(U, Amarillo, Ciclo, Fin_rojo, Fin_amarillo)).
%-------------- FIN dragones comiendo------------------------------

%----------------Los dragones se reproducen--------------------------
%Reproduccion - Reproduccion(Temperatura, Dragon)
reproducir(Temperatura, Dragon):-
    Dragon==dragon_amarillo ->
    (
        contador(Rojo, Amarillo, Ciclo, Fin_rojo, Fin_amarillo),
        fertilidad(Temperatura, Dragon, Fertilidad),
        U is Amarillo * Fertilidad,
        retractall(contador(Rojo, _, Ciclo, Fin_rojo, Fin_amarillo)),
        assert(contador(Rojo, U, Ciclo, Fin_rojo, Fin_amarillo))
    );
    contador(Rojo, Amarillo, Ciclo, Fin_rojo, Fin_amarillo),
    fertilidad(Temperatura, Dragon, Fertilidad),
    V is Rojo * Fertilidad,
    retractall(contador(_, Amarillo, Ciclo, Fin_rojo, Fin_amarillo)),
    assert(contador(V, Amarillo, Ciclo, Fin_rojo, Fin_amarillo)).

%Amalisis de las condiciones de reproducion
%Cada especie se reproduce con distinta frecuencia
condicion_reproduccion(Temperatura):- 
    condicion_naranja(Temperatura);
    condicion_roja(Temperatura);
    condicion_amarillo(Temperatura).

condicion_naranja(Temperatura) :-
    contador(_, _, Ciclo, _, _),
    frecuencia(Temperatura, dragon_rojo, Frecuencia_roja),
    frecuencia(Temperatura, dragon_amarillo, Frecuencia_amarilla),
    Frecuencia_naranja is Frecuencia_roja * Frecuencia_amarilla,
    not(0 is Ciclo),
    0 is mod(Ciclo, Frecuencia_naranja),
    reproducir(Temperatura, dragon_rojo),
    reproducir(Temperatura, dragon_amarillo),
    contar_ciclo.

condicion_roja(Temperatura) :-
    contador(_, _, Ciclo, _, _),
    frecuencia(Temperatura, dragon_rojo, Frecuencia_roja),
    not(0 is Ciclo),
    0 is mod(Ciclo, Frecuencia_roja),
    reproducir(Temperatura, dragon_rojo),
    contar_ciclo.

condicion_amarillo(Temperatura) :-
    contador(_, _, Ciclo, _, _),
    frecuencia(Temperatura, dragon_amarillo, Frecuencia_amarilla),
    not(0 is Ciclo),
    0 is mod(Ciclo, Frecuencia_amarilla),
    reproducir(Temperatura, dragon_amarillo),
    contar_ciclo.
%----------------FIN Los dragones se reproducen--------------------------

%Bucles - Recursividad - simula el paso del tiempo
tiempo(_, 0):- estado.
tiempo(Temperatura, X) :-
    comer(Temperatura),
    tic(Temperatura),
    Y is X - 1,
    tiempo(Temperatura, Y).

%Auxiliar
tic(Temperatura) :- 
    estado,
    condicion_reproduccion(Temperatura);
    contar_ciclo.

%Bucles - Recursividad - Cantidad comida por especie / Ciclo
comerAR_cantidad(_, 0).
comerAR_cantidad(Temperatura, X) :- 
    comer(Temperatura, dragon_amarillo, dragon_rojo),
    Y is X - 1,
    comerAR_cantidad(Temperatura, Y).

comerAA_cantidad(_, 0).
comerAA_cantidad(Temperatura, X) :- 
    comer(Temperatura, dragon_amarillo, dragon_amarillo),
    Y is X - 1,
    comerAA_cantidad(Temperatura, Y).

%---------------------- Main ------------------------
%Comportamiento del simulador
main:-
    clear,
    mensaje_presentacion,
    continuar,
    mensaje_opciones,
    estado0,
    temp(Temp),
    Temperatura is Temp,
    ciclo5(Temperatura).

% Auxiliares main
% Desencadena la simulacion segun la opcion elegida por el usuario
set_inicio(Opcion) :-
    Opcion==1 -> 
    (
        inicio(_, _),
        reset(estrella1, (R1, A1)),
        retractall(inicio(_, _)),
        assert(inicio(R1, A1)),
        temp(_),
        descripcion(estrella1, (_, _, Temp1)),
        retractall(temp(_)),
        assert(temp(Temp1))
    );
    Opcion==2 -> 
    (
        inicio(_, _),
        reset(estrella2, (R2, A2)),
        retractall(inicio(_, _)),
        assert(inicio(R2, A2)),
        temp(_),
        descripcion(estrella2, (_, _, Temp2)),
        retractall(temp(_)),
        assert(temp(Temp2))
    );
    Opcion==3 -> 
    (
        inicio(_, _),
        reset(estrella3, (R3, A3)),
        retractall(inicio(_, _)),
        assert(inicio(R3, A3)),
        temp(_),
        descripcion(estrella3, (_, _, Temp3)),
        retractall(temp(_)),
        assert(temp(Temp3))
    ).

% Facilita la lectura de los mensajes por consola- Auxiliar
continuar :-
    write("Escriba 'C.' y pulse intro para continuar: "),
    read(X),
    write(X).

% Solicita al usuario si simular 5 ciclos más o terminar
% la simulaciom - Auxiliar
ciclo5(Temperatura) :-
    nl,nl,
    write("Si desea simular 5 ciclos del tiempo solar,"),nl,
    write("Introduzca '5.' y pulse intro,"), nl,
    write("Si introduce otro numero, la simulacion finaliza."), nl,
    read(X),
    (
        X==5 -> tiempo(Temperatura, 5), ciclo5(Temperatura);
        nl, nl, write("** LA SIMULACION HA TERMINADO **"), nl, nl
    )
.

% Facilita la lectura de los mensajes por consola- Auxiliar
%Limpia consola
clear :- write('\e[2J').

%-------- Menu. Mensajes. ----------------------------
mensaje_presentacion :-
    write(" ===================================================================="), nl,
    write(" ** SIMULADOR DEL ECOSISTEMA SOLAR EN DISTINTOS TIPOS DE ESTRELLAS **"), nl,
    write(" ===================================================================="), nl, nl,
    write("El nucleo solar se encuentra habitado por dos tipos de dragones."), nl,
    write("Dragones Rojos"), nl,
    write("Dragones Amarillos. Depredadores."), nl, nl,
    write("A continuacion, podra simular la evolucion de la poblacion de dragones"), nl,
    write("en los ecosistemas que proporcionan distintos tipos de estrellas."), nl, nl.

mensaje_opciones :-
    clear,
    write("El sol es una estrella 'enana amarilla' con temperatura: 5800 K"), nl,
    write("La poblacion de dragones que habita su nucleo es estable."), nl, nl,
    write("Elija la opcion en la que desea simular la evolucion de la poblacion de dragones."), nl, nl,
    write("*OPCION 1*"), nl,
    write("----------"), nl,
    descripcion(estrella1, (Nombre1, Tipo1, T1)),
    write("Estrella: "), write(Nombre1), write(". Tipo: "), write(Tipo1), write(". Temperatura: "), write(T1), write(" k."), nl,
    write("Pulse '1.' e intro."), nl, nl,
    write("*OPCION 2*"), nl,
    write("----------"), nl,
    descripcion(estrella2, (Nombre2, Tipo2, T2)),
    write("Estrella: "), write(Nombre2), write(". Tipo: "), write(Tipo2), write(". Temperatura: "), write(T2), write(" k."), nl,
    write("Pulse '2.' e intro."), nl, nl,
    write("*OPCION 3*"), nl,
    write("----------"), nl,
    descripcion(estrella3, (Nombre3, Tipo3, T3)),
    write("Estrella: "), write(Nombre3), write(". Tipo: "), write(Tipo3), write(". Temperatura: "), write(T3), write(" k."), nl,
    write("Pulse '3.' e intro."), nl, nl,
    read(Opcion),
    clear,
    write("Selecciono la opcion: "), write(Opcion), nl,
    (
        opcion_correcta(Opcion) -> set_inicio(Opcion);
        mensaje_opciones
    ).

%Chequeo opcion
opcion_correcta(Opcion) :-
    Opcion==1;Opcion==2;Opcion==3.