defmodule Pollutiondb.Repo.Migrations.CreateReadings do
  use Ecto.Migration

  def change do
    create table(:readings) do
      add :station_id, references(:stations, on_delete: :delete_all)
      add :date, :date
      add :time, :time
      add :type, :string
      add :value, :float
    end
  end
end
