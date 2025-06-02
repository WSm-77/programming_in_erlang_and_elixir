defmodule Pollutiondb.Station do
  use Ecto.Schema

  schema "stations" do
      field :name, :string
      field :lon, :float
      field :lat, :float
  end

  def add(station) do
    Pollutiondb.Repo.insert(station)
  end

  def get_all() do
    Pollutiondb.Repo.all(Pollutiondb.Station)
  end

  def get_by_id(id) do
    Pollutiondb.Repo.get(Pollutiondb.Station, id)
  end

  def remove(station) do
    Pollutiondb.Repo.delete(station)
  end
end
