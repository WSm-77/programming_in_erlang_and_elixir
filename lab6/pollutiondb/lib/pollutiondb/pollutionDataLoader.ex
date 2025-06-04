defmodule PollutionDataLoader do
  @moduledoc """
  Module providing utility methods for loading and manipulationg data used later
  on by pollution server.
  """

  @doc """
  Parses single line of data from .csv file

  ## Example:

      iex> PollutionDataLoader.parse_line("2024-02-10T09:00:00.000Z;PM1;17.07;57570;Polska, Kraków, Floriana Straszewskiego;50.057224,19.933157")
      %{
        location: {50.057224, 19.933157},
        datetime: {{2024, 2, 10}, {9, 0, 0}},
        pollutionType: "PM1",
        pollutionLevel: 17.07,
        stationId: 57570,
        stationName: "Polska, Kraków, Floriana Straszewskiego"
      }
  """
  def parse_line(line) do
    line |> String.split(";")
    [datetime, pollutionType, pollutionLevel, stationId, stationName, location] = line |> String.split(";")
    location = location |>
               String.split(",") |>
               Enum.map(&String.to_float/1) |>
               List.to_tuple
    pollutionLevel = String.to_float(pollutionLevel)
    stationId = String.to_integer(stationId)
    {:ok, datetime, _offset} = DateTime.from_iso8601(datetime)
    datetime = DateTime.to_naive(datetime) |>
               NaiveDateTime.to_erl
    %{
      datetime: datetime,
      location: location,
      stationId: stationId,
      stationName: stationName,
      pollutionType: pollutionType,
      pollutionLevel: pollutionLevel
    }
  end

  @doc """
  Identifies unique stations from pollution data.

  ## Example:

      iex> PollutionDataLoader.identify_stations([
      ...>   %{
      ...>     location: {50.057224, 19.933157},
      ...>     datetime: {{2024, 2, 10}, {9, 0, 0}},
      ...>     pollutionType: "PM1",
      ...>     pollutionLevel: 17.07,
      ...>     stationId: 57570,
      ...>     stationName: "Polska, Kraków, Floriana Straszewskiego"
      ...>   },
      ...>   %{
      ...>     location: {50.057224, 19.933157},
      ...>     datetime: {{2024, 2, 10}, {9, 0, 0}},
      ...>     pollutionType: "PM25",
      ...>     pollutionLevel: 27.92,
      ...>     stationId: 57570,
      ...>     stationName: "Polska, Kraków, Floriana Straszewskiego"
      ...>   },
      ...>   %{
      ...>     location: {0.0, 0.0},
      ...>     datetime: {{2024, 2, 10}, {9, 0, 0}},
      ...>     pollutionType: "PM25",
      ...>     pollutionLevel: 27.92,
      ...>     stationId: 1,
      ...>     stationName: "Polska, Warszawa, Praga"
      ...>   }
      ...> ])
      [
        %{
          location: {50.057224, 19.933157},
          stationId: 57570,
          stationName: "Polska, Kraków, Floriana Straszewskiego"
        },
        %{
          location: {0.0, 0.0},
          stationId: 1,
          stationName: "Polska, Warszawa, Praga"
        }
      ]
  """
  def identify_stations(data_lines) when is_list(data_lines) do
    data_lines |>
    Enum.uniq_by(fn (data_map) -> data_map.stationName end) |>
    Enum.uniq_by(fn (data_map) -> data_map.location end) |>
    Enum.uniq_by(fn (data_map) -> data_map.stationId end) |>
    Enum.map(
      fn (data_map) ->
        %{location: data_map.location,
          stationName: data_map.stationName,
          stationId: data_map.stationId}
      end
      )
  end

  def identify_stations(_), do: {:error, "Ivalid arguments"}

  defp add_stations(data_lines) do
    data_lines
    |> PollutionDataLoader.identify_stations
    |> Enum.each(
      fn (station) ->
        IO.inspect(station)
        {lat, lon} = station.location
        Pollutiondb.Station.add(
          "#{station.stationName} #{station.stationId}",
          lon,
          lat
        )
      end
    )
  end

  defp add_values(data_lines) do
    data_lines
    |> Enum.each(
      fn (data) ->
        {date_erl, time_erl} = data.datetime
        {_, date} = Date.from_erl(date_erl)
        {_, time} = Time.from_erl(time_erl)

        case Pollutiondb.Station.find_by_name("#{data.stationName} #{data.stationId}") do
          nil -> nil
          station -> Pollutiondb.Reading.add(station, date, time, data.pollutionType, data.pollutionLevel)
        end
      end
    )
  end

  def load_from_csv(path) do
    data_lines = File.read!(path)
      |> String.trim_trailing
      |> String.split("\n")
      |> Enum.map(&PollutionDataLoader.parse_line/1)

    add_stations(data_lines)
    add_values(data_lines)
  end
end
