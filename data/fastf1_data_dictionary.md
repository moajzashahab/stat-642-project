# `all_laps.csv` (lap-level data)

## Timing

* **Time** → elapsed time in session (timedelta, not a date)
* **LapTime** → duration of that lap
* **LapNumber** → lap index within session
* **LapStartTime** → elapsed session time when lap started
* **LapStartDate** → actual timestamp (real date)

## Sector breakdown

* **Sector1Time / Sector2Time / Sector3Time** → time per sector
* **Sector1SessionTime / Sector2SessionTime / Sector3SessionTime** → when each sector was completed (relative to session)

## Speed

* **SpeedI1 / SpeedI2** → intermediate speed trap points
* **SpeedFL** → finish line speed
* **SpeedST** → main straight speed trap

## Tire info

* **Compound** → tire type (Soft, Medium, Hard, etc)
* **TyreLife** → laps on that tire
* **FreshTyre** → whether tire was new

## Driver / team

* **Driver** → driver code (VER, HAM, etc)
* **DriverNumber** → car number
* **Team** → team name

## Pit / stint

* **Stint** → which stint (sequence between pit stops)
* **PitInTime / PitOutTime** → when pit occurred

## Flags / quality

* **IsPersonalBest** → whether lap is driver’s fastest
* **Deleted** → lap invalidated
* **DeletedReason** → why it was deleted
* **IsAccurate** → data reliability flag
* **FastF1Generated** → whether derived by FastF1

## Track / race context

* **TrackStatus** → track condition (green, yellow, etc)
* **Position** → position at that lap

## Event info

* **EventName**
* **EventDate**


# `all_results.csv` (session summary per driver)

## Driver identity

* **DriverId / DriverNumber / Abbreviation**
* **FirstName / LastName / FullName**
* **BroadcastName** → TV format name
* **HeadshotUrl** → image link
* **CountryCode**

## Team info

* **TeamName**
* **TeamId**
* **TeamColor**

## Performance

* **Position** → finishing position in session
* **ClassifiedPosition** → official classification
* **GridPosition** → starting position
* **Q1 / Q2 / Q3** → qualifying times

## Race/session stats

* **Time** → total time
* **Status** → finished, DNF, etc.
* **Points**
* **Laps** → number of laps completed

## Event info

* **EventName**
* **EventDate**


# `all_weather.csv` (time-series weather)

## Timing

* **Time** → elapsed session time (again, timedelta)

## Weather conditions

* **AirTemp** → air temperature
* **TrackTemp** → track temperature
* **Humidity**
* **Pressure**
* **Rainfall** → boolean/indicator
* **WindDirection**
* **WindSpeed**

## Event info

* **EventName**
* **EventDate**
