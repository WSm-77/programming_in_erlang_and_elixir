defmodule PollutiondbWeb.StationLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket, stations: Station.get_all(), name: "", lat: "", lon: "")
    {:ok, socket}
  end

  def render(assigns) do
    ~H"""
    Create new station
    <form phx-submit="insert">
      Name: <input type="text" name="name" value={@name} /><br/>
      Lat: <input type="number" name="lat" step="0.1" value={@lat} /><br/>
      Lon: <input type="number" name="lon" step="0.1" value={@lon} /><br/>
      <input type="submit" />
    </form>

    <table class="min-w-full border-collapse border border-gray-200 shadow-lg bg-white">
      <thead class="bg-blue-100 sticky top-0">
        <tr>
          <th>Name</th><th>Longitude</th><th>Latitude</th>
        </tr>
      </thead>
      <tbody class="divide-y divide-gray-200">
        <%= for station <- @stations do %>
          <tr class="hover:bg-gray-200">
            <td><%= station.name %></td>
            <td><%= station.lon %></td>
            <td><%= station.lat %></td>
          </tr>
        <% end %>
      </tbody>
    </table>
    """
  end
end
