% Parcial Who you gonna call?

%herramientasRequeridas(TareaDeLimpieza, Herramientas).
herramientasRequeridas(ordenarCuarto, [aspiradora(100), trapeador, plumero]).
herramientasRequeridas(limpiarTecho, [escoba, pala]).
herramientasRequeridas(cortarPasto, [bordedadora]).
herramientasRequeridas(limpiarBanio, [sopapa, trapeador]).
herramientasRequeridas(encerarPisos, [lustradpesora, cera, aspiradora(300)]).

% aspiradora(Potencia Minima).

%los cazafantasmas (Peter, Egon, Ray y Winston)

% 1) Agregar a la base de conocimientos la siguiente información:
% - Egon tiene una aspiradora de 200 de potencia.
% - Egon y Peter tienen un trapeador, Ray y Winston no.
% - Sólo Winston tiene una varita de neutrones.
% - Nadie tiene una bordeadora.

tiene(egon, aspiradora(200)).
tiene(egon, trapeador).
tiene(peter, trapeador).
tiene(winston, varitaDeNeutrones).

% 2) Definir un predicado que determine si un integrante satisface 
% la necesidad de una herramienta requerida. Esto será cierto si 
% tiene dicha herramienta, teniendo en cuenta que si la herramienta
% requerida es una aspiradora, el integrante debe tener una con
% potencia igual o superior a la requerida.
% Nota: No se pretende que sea inversible respecto a la herramienta
% requerida.

satisfaceLaNecesidad(Persona, Herramienta) :-
    tiene(Persona, Herramienta).

satisfaceLaNecesidad(Persona, aspiradora(PotenciaRequerida)) :-
    tiene(Persona, aspiradora(Potencia)),
    % Potencia > PotenciaRequerida.  NO INVERSIBLE hacia PotenciaRequerida
    between(0, Potencia, PotenciaRequerida).
    
% 3) Queremos saber si una persona puede realizar una tarea, 
% que dependerá de las herramientas que tenga. Sabemos que:
% - Quien tenga una varita de neutrones puede hacer cualquier tarea, independientemente de qué herramientas requiera dicha tarea.
% - Alternativamente alguien puede hacer una tarea si puede satisfacer la necesidad de todas las herramientas requeridas para dicha tarea. 

puedeRealizarTarea(Persona, Tarea) :-
    herramientasRequeridas(Tarea, _),
    tiene(Persona, varitaDeNeutrones).

puedeRealizarTarea(Persona, Tarea) :-
    esPersona(Persona),
    herramientasRequeridas(Tarea, Herramientas),
    forall((member(Herramienta, Herramientas)), satisfaceLaNecesidad(Persona, Herramienta)).
    
puedeRealizarTareaV2(Persona, Tarea) :-
    herramientasRequeridas(Tarea, _),
    tiene(Persona, varitaDeNeutrones).

puedeRealizarTareaV2(Persona, Tarea) :-
    esPersona(Persona),
    requiereHerramienta(Tarea, _),
    forall(requiereHerramienta(Tarea, Herramienta), satisfaceLaNecesidad(Persona, Herramienta)).

esPersona(Persona) :- tiene(Persona, _).

requiereHerramienta(Tarea, Herramienta) :-
    herramientasRequeridas(Tarea, Herramientas),
    member(Herramienta, Herramientas).

% 4) ¿Cuanto se le debería cobrar a un cliente por un pedido (que son las tareas que pide).?

% - tareaPedida/3: relaciona al cliente, con la tarea pedida y la cantidad de metros cuadrados sobre los cuales hay que realizar esa tarea.
% - precio/2: relaciona una tarea con el precio por metro cuadrado que se cobraría al cliente.
% Entonces lo que se le cobraría al cliente sería la suma del valor a cobrar por cada tarea, multiplicando el precio por los metros cuadrados de la tarea.

%tareaPedida(tarea, cliente, metrosCuadrados).
tareaPedida(ordenarCuarto, dana, 20).
tareaPedida(cortarPasto, dana, 20).
tareaPedida(cortarPasto, walter, 50).
tareaPedida(limpiarTecho, walter, 70).
tareaPedida(limpiarBanio, louis, 15).

% precio(TareaPedida, PrecioPorMetroCuadrado).
precio(ordenarCuarto, 13).
precio(limpiarTecho, 20).
precio(limpiarBanio, 55).
precio(cortarPasto, 10).
precio(encerarPisos, 7).

% un cliente tiene un pedido (una lista de tareas) y se debe conocer el precio de todo ese pedido
precioACobrar(Cliente, PrecioACobrar) :-
    tareaPedida(_, Cliente, _),
    findall(Precio, (tareaPedida(Tarea, Cliente, _), precioPorTareaPedida(Cliente, Tarea, Precio)), Precios),
    sum_list(Precios, PrecioACobrar).

precioPorTareaPedida(Cliente, Tarea, Precio):-
    tareaPedida(Tarea, Cliente, CantidadDeMetrosCuadrados),
    precio(Tarea, PrecioPorMetroCuadrado),
    Precio is PrecioPorMetroCuadrado * CantidadDeMetrosCuadrados.

% 5) necesitamos saber quiénes aceptarían el pedido de un cliente. Un integrante acepta el pedido cuando puede realizar todas las tareas del pedido y además está dispuesto a aceptarlo.
% Sabemos que Ray sólo acepta pedidos que no incluyan limpiar techos, Winston sólo acepta pedidos que paguen más de $500, Egon está dispuesto a aceptar pedidos que no tengan tareas complejas y Peter está dispuesto a aceptar cualquier pedido.
% Decimos que una tarea es compleja si requiere más de dos herramientas. Además la limpieza de techos siempre es compleja

aceptaElPedido(Trabajador, Cliente) :-
    puedeHacerPedido(Trabajador, Cliente),
    estaDispuestoAAceptarlo(Trabajador, Cliente).

puedeHacerPedido(Trabajador, Cliente) :-
    tiene(Trabajador, _),
    tareaPedida(_, Cliente, _),
    forall(tareaPedida(Tarea,Cliente,_), 
    puedeRealizarTarea(Trabajador, Tarea)).

estaDispuestoAAceptarlo(ray, Cliente) :- 
    %tareaPedida(Pedido, Cliente, _),
    %Pedido \= limpiarTecho.
    not(tareaPedida(limpiarTecho, Cliente, _)).

estaDispuestoAAceptarlo(winston, Cliente) :-
    precioACobrar(Cliente, Precio),
    Precio >= 500.

estaDispuestoAAceptarlo(egon, Cliente) :-
    %tareaPedida(Pedido, Cliente, _),
    %not(esCompleja(Pedido)).
    not((tareaPedida(Pedido, Cliente, _), esCompleja(Pedido))).

estaDispuestoAAceptarlo(peter, _).

esCompleja(Tarea) :- 
    herramientasRequeridas(Tarea, ListaDeHerramientas),
    length(ListaDeHerramientas, Cantidad),
    Cantidad > 2.

esCompleja(limpiarTecho).

% 6) Necesitamos agregar la posibilidad de tener herramientas reemplazables, que incluyan 2 herramientas de las que pueden tener los 
% integrantes como alternativas, para que puedan usarse como un requerimiento para poder llevar a cabo una tarea

% a) Mostrar cómo modelarías este nuevo tipo de información modificando el hecho de herramientasRequeridas/2 para que ordenar 
% un cuarto pueda realizarse tanto con una aspiradora de 100 de potencia como con una escoba, además del trapeador y el plumero que ya eran necesarios.




% b) Realizar los cambios/agregados necesarios a los predicados definidos en los puntos anteriores para que se soporten estos nuevos requerimientos de herramientas para poder llevar a cabo una tarea, teniendo en cuenta que para las herramientas reemplazables alcanza con que el integrante satisfaga la necesidad de alguna de las herramientas indicadas para cumplir dicho requerimiento.
% c) Explicar a qué se debe que esto sea difícil o fácil de incorporar.

    