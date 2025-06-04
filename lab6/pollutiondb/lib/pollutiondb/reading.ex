defmodule Pollutiondb.Reading do
  require Ecto.Query
  use Ecto.Schema

  schema "readings" do
    field :date, :date
    field :time, :time
    field :type, :string
    field :value, :float
    belongs_to :station, Pollutiondb.Station
  end

  def add_now(station, type, value) do
    Pollutiondb.Repo.insert(%Pollutiondb.Reading{
      station_id: station.id,
      date: Date.utc_today(),
      time: Time.utc_now(),
      type: type,
      value: value
    })
  end

  def add(station, date, time, type, value) do
    Pollutiondb.Repo.insert(%Pollutiondb.Reading{
      station_id: station.id,
      date: date,
      time: time,
      type: type,
      value: value
    })
  end

  def find_by_date(date) do
    Ecto.Query.from(reading in Pollutiondb.Reading,
      where: reading.date == ^date
    ) |> Pollutiondb.Repo.all
  end
end
