
%USES DATE AS ONE COLUMN FROM CSV
-module(main).
-export([start/0]).
-define(FILENAME, "flights_info.csv").

-define(DATE, 1).
-define(AIRLINE_CODE, 2).
-define(AIRLINE_NAME, 3). 
-define(FLIGHT_NUM, 4). 
-define(ORIGIN_AIRPORT, 5). 
-define(ORIGIN_NAME, 6).
-define(ORIGIN_CITY, 7).
-define(ORIGIN_STATE, 8).
-define(DEST_AIRPORT, 9). 
-define(DEST_NAME, 10).
-define(DEST_CITY, 11).
-define(DEST_STATE, 12).
-define(DEPARTURE_SCHEDULED, 13). 
-define(DEPARTURE_TIME, 14). 
-define(DEPARTURE_DELAY, 15). 
-define(ARRIVAL_SCHEDULED, 16). 
-define(ARRIVAL_TIME, 17). 
-define(ARRIVAL_DELAY, 18). 

start() -> flights_file(?FILENAME).

flights_file(Filename) ->
	case file:open(Filename, read) of
		{ok, IoDevice} ->
		  % PROCESS CSV	
      {ok, Airline_Map, Airport_Map, Date_Map} = process_data(IoDevice, #{}, #{}, #{}),
      % TEST CASE I/O CHECKING
      {ok, Test_Case} = io:read(""),
      io:format("test case ~B~n", [Test_Case]),
      test(Test_Case, Airline_Map, Airport_Map, Date_Map);
	  {error, Reason} ->
		  io:format("~s~n", [Reason])
	end.

% Process CSV
process_data(IoDevice, Airline_Map, Airport_Map, Date_Map) ->
	case io:get_line(IoDevice,"") of
		% End of file
    eof -> 
			file:close(IoDevice),
			{ok, Airline_Map, Airport_Map, Date_Map};
		{error, Reason} ->
			file:close(IoDevice),
			throw(Reason);
		% Each row
    Line ->
			Data = case re:run(Line, "\"[^\"]*\"|[^,]+", [{capture,first, list},global]) of   % doesn't handle empty fields
				{match, Captures} -> [ hd(C) || C <- Captures];
				nomatch -> []
		  end,
      % Data is a list of fields from Line
%%        io:format("~p~n", [list_to_tuple(Data)]),
      % YOUR CODE GOES HERE! Pseudocode given below
	  My_Data = list_to_tuple(Data),
      
	 {Flight_date,
		Airline_code, 
		Airline_name,
		Flight_number,
		Origin_airport, 
		Origin_name,
		Origin_city, 
		Origin_state,
		Destination_airport,
		Destination_name, 
		Destination_city,
		Destination_state,
		Scheduled_departure,
		Departure_time,
		Departure_delay,
		Scheduled_arrival,
		 Arrival_time,
		Arrival_delay
		 } = My_Data,
	  % Flight tuple 
		Flight_tuple ={	
					   flight,
					   Airline_code,
					   	Flight_number,
					   Origin_airport,
					   Destination_airport,
					   Scheduled_departure,
					   Departure_time,
					   Departure_delay,
					   Scheduled_arrival,
					   Arrival_time,
					   Arrival_delay
					   },
      % Process flight in Date map
	  case maps:is_key(Flight_date, Date_Map) of  
		true ->	
			 Val =  maps:get(Flight_date, Date_Map) ,
			Date_Map_Updated = maps:update(Flight_date,Val ++ [Flight_tuple], Date_Map);
	   false -> 
		    Date_Map_Updated = maps:put(Flight_date, [Flight_tuple], Date_Map)
		end,
      % Airline tuple 
		Airline_tuple = {  airline,
						  Airline_code,
						 Airline_name,
						 [Flight_tuple]
						 },
      % Process airline in Airline map
	case maps:is_key(Airline_code, Airline_Map) of
		true ->
			{airline,Air_code,Air_Name, FtuplesList} =  maps:get(Airline_code, Airline_Map) ,
			Airline_Map_Updated = maps:update(Airline_code,{airline,Air_code,Air_Name,FtuplesList ++ [Flight_tuple] }, Airline_Map);
		false ->
			Airline_Map_Updated = maps:put(Airline_code, Airline_tuple, Airline_Map)
			end,
      % Airport: ORIGIN
		Origin_Airport_tuple = {airport,
						  Origin_airport,
						  Origin_name,
					      Origin_city, 
						  Origin_state,
						  [Flight_tuple],
						  []},
      % Process origin airport in Airport map
		case maps:is_key(Origin_airport, Airport_Map) of
		true ->
			{_,_,_,_,_,ORIGIN,DEST} =  maps:get(Origin_airport, Airport_Map) ,
			Airport_Map_Updated = maps:update(Origin_airport,{airport,
						  Origin_airport,
						  Origin_name,
					      Origin_city, 
						  Origin_state,
						  ORIGIN ++ [Flight_tuple],
						  DEST}, Airport_Map);
		false ->
			Airport_Map_Updated = maps:put(Origin_airport, Origin_Airport_tuple, Airport_Map)
			end,
      % Airport: DEST
		Dest_Airport_tuple = {airport,
						  Destination_airport,
							Destination_name, 
							Destination_city,
							Destination_state,
							[] ,
						   [Flight_tuple]},
      % Process destination airport in Airport map
case maps:is_key(Destination_airport, Airport_Map_Updated) of
		true ->
			{_,_,_,_,_,ORIGIN_2,DEST_2} =  maps:get(Destination_airport, Airport_Map_Updated) ,
			Airport_Map_Updated_2 = maps:update(Destination_airport,{airport,
						 Destination_airport,
							Destination_name, 
							Destination_city,
							Destination_state,
						  ORIGIN_2 ,
						  DEST_2 ++ [Flight_tuple]}, Airport_Map_Updated);
		false ->
			Airport_Map_Updated_2 = maps:put(Destination_airport, Dest_Airport_tuple, Airport_Map_Updated)
			end,
      % Recursive call - to be changed to updated maps
      process_data(IoDevice, Airline_Map_Updated, Airport_Map_Updated_2, Date_Map_Updated)
  end.




% Query 1
query1(_Date_Map) ->
	  solve1(maps:to_list(_Date_Map)),
	ok.
solve1([]) -> ok;
solve1([H | T]) ->
	Date_Tuple_to_List = tuple_to_list(H),
	Key =lists:nth(1, Date_Tuple_to_List),
	io:format("~p ~n", [Key]),
	Value = lists:nth(2, Date_Tuple_to_List),
	S = [lists:nth(11, (tuple_to_list(TmpN))) || TmpN <- Value ],
	D = [ list_to_integer (string:trim(C))  || C <- S ] ,
	Max_Arrival_Delay = lists:max(D) ,
	[io:format("~p~n", [Other_Tmp]) || 
					Other_Tmp <- Value 	,
 list_to_integer (string:trim(lists:nth(11, (tuple_to_list(Other_Tmp))) )) ==  Max_Arrival_Delay],
	solve1(T).




% Query 2
query2(_Airport_Map) ->
	solve2(maps:values(_Airport_Map)),
	ok.

solve2([]) -> ok;
solve2([H | T]) ->
	{airport, Code, Name, City, State, OriginFlights, DestFlights} = H,
	 io:format("{~s, ~s, ~s, ~s, ~B, ~B}~n", [Code, Name, City, State, length(OriginFlights), length(DestFlights)]),
  	 Origin_Delays = [list_to_integer (lists:nth(8, (tuple_to_list(X))) ) || X <- OriginFlights  ],
	Num_Origin_Delays = [X || X <- Origin_Delays , X > 15],
	Dest_Delays = [list_to_integer (string:trim(lists:nth(11, (tuple_to_list(X)))) ) || X <- DestFlights  ],
	Num_Dest_Delays = [X || X <- Dest_Delays , X > 15],
	io:format("origin delays: ~B~n",[length(Num_Origin_Delays)]),
	io:format("destination delays: ~B~n",[length(Num_Dest_Delays)]),
	solve2(T).




% TEST CASES - DO NOT CHANGE CODE BELOW FOR INPUT-OUTPUT TESTING
test(1, Airline_Map, Airport_Map, Date_Map) -> 
  testMaps(lists:sort(maps:values(Airline_Map))),
  testMaps(lists:sort(maps:values(Airport_Map))),
  testMaps(lists:sort(maps:keys(Date_Map)), Date_Map);
test(2, _, _, Date_Map) ->
  query1(Date_Map);
test(3, _, Airport_Map, _) ->
  query2(Airport_Map).

% Test case 1
testMaps([]) -> ok;
testMaps([{airline, Code, Name, Flights}|Tail]) ->
  io:format("{~s, ~s, ~B}~n", [Code, Name, length(Flights)]),
  testMaps(Tail);
testMaps([{airport, Code, Name, City, State, OriginFlights, DestFlights}|Tail]) ->
  io:format("{~s, ~s, ~s, ~s, ~B, ~B}~n", [Code, Name, City, State, length(OriginFlights), length(DestFlights)]),
  testMaps(Tail).

testMaps([], _) -> ok;
testMaps([Date|Tail], Date_Map) ->
  Flights = maps:get(Date, Date_Map),
  io:format("~s: ~B~n", [Date, length(Flights)]),
  io:format("~p~n", [lists:nth(1, Flights)]),
  io:format("~p~n", [lists:nth(length(Flights), Flights)]),
  testMaps(Tail, Date_Map).

