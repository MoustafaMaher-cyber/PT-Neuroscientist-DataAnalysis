import time
import pandas as pd
import numpy as np

CITY_DATA = { 'chicago': 'chicago.csv',
              'new york city': 'new_york_city.csv',
              'washington': 'washington.csv' }

def get_filters():
    """
    Asks user to specify a city, month, and day to analyze.

    Returns:
        (str) city - name of the city to analyze
        (str) month - name of the month to filter by, or "all" to apply no month filter
        (str) day - name of the day of week to filter by, or "all" to apply no day filter
    """
    print('Hello! Let\'s explore some US bikeshare data!')
    # TO DO: get user input for city (chicago, new york city, washington). HINT: Use a while loop to handle invalid inputs
    while True:
        city = input("\n choose city : new york city , chicago ,washington \n").title()
    if city == 'Chicago' or city == 'C':
        return 'chicago.csv'
        continue
    elif city == 'New York' or city == "N":
        return 'new_york_city.csv'
        continue
    elif city == 'Washington' or city == 'W':
        return 'washington.csv'
        continue
    else:
        print("\n enter a valid city")
        break

    # TO DO: get user input for month (all, january, february, ... , june)
    while True:
        month = input("\nchoose month: january, february, march, april, may, june or type \"all\" for all months\n").lower()
        if month not in ('january', 'february', 'march', 'april', 'may', 'june', 'all'):
             print("choose an existed month , try again")
             continue
        else:
             break

    # TO DO: get user input for day of week (all, monday, tuesday, ... sunday)
    while True:
      day = input("\n if you want choose a day: sunday, monday, tuesday, wednesday, thursday, friday, saturday or type \"all\" if you do not have any preference.\n")
      if day not in ('sunday', 'monday', 'tuesday', 'wednesday', 'thursday', 'friday', 'saturday', 'all'):
        print("Sorry, I didn't catch that. Try again.")
        continue
      else:
        break

    print('-'*40)
    return city, month, day


def load_data(city, month, day):
    """
    Loads data for the specified city and filters by month and day if applicable.

    Args:
        (str) city - name of the city to analyze
        (str) month - name of the month to filter by, or "all" to apply no month filter
        (str) day - name of the day of week to filter by, or "all" to apply no day filter
    Returns:
        df - Pandas DataFrame containing city data filtered by month and day
    """
    df = pd.read_csv(CITY_DATA[city])
    df['Start Time'] = pd.to_datetime(df['Start Time'])
    df['month'] = df['Start Time'].dt.month
    df['day_of_week'] = df['Start Time'].dt.weekday_name
    if month != 'all':
        months = ['january', 'february', 'march', 'april', 'may', 'june']
        month = months.index(month) + 1
        df = df[df['month'] == month]
    if day != 'all':
        df = df[df['day_of_week'] == day.title()]
    return df


def time_stats(df):
    """Displays statistics on the most frequent times of travel."""

    print('\nCalculating The Most Frequent Times of Travel...\n')
    start_time = time.time()

    # TO DO: display the most common month
    popularmonth = df['month'].mode()[0]
    print('common month:', popularmonth)

    # TO DO: display the most common day of week
    popularday = df['day_of_week'].mode()[0]
    print('common day:', popularday)

    # TO DO: display the most common start hour
    df['hour'] = df['Start Time'].dt.hour
    popularhour = df['hour'].mode()[0]
    print(' common hour :', popularhour)

    print("\nThis took %s seconds." % (time.time() - start_time))
    print('-'*40)


def station_stats(df):
    """Displays statistics on the most popular stations and trip."""

    print('\nCalculating The Most Popular Stations and Trip...\n')
    start_time = time.time()

    # TO DO: display most commonly used start station
    StartStation = df['Start Station'].value_counts().idxmax()
    print('\n common start station:', StartStation)

    # TO DO: display most commonly used end station
    EndStation = df['End Station'].value_counts().idxmax()
    print('\n common end station:', EndStation)

    # TO DO: display most frequent combination of start station and end station trip
    CombinationStation = df.groupby(['Start Station', 'End Station']).count()
    print('\nCommon combination of start station and end station trip:', StartStation, " & ", EndStation)

    print("\nThis took %s seconds." % (time.time() - start_time))
    print('-'*40)


def trip_duration_stats(df):
    """Displays statistics on the total and average trip duration."""

    print('\nCalculating Trip Duration...\n')
    start_time = time.time()

    # TO DO: display total travel time
    TotalTravelTime = sum(df['Trip Duration'])
    print('total travel time:', TotalTravelTime/86400,"days")

    # TO DO: display mean travel time
    MeanTravelTime = df['Trip Duration'].mean()
    print('mean travel time:', MeanTravelTime/60,"minutes")

    print("\nThis took %s seconds." % (time.time() - start_time))
    print('-'*40)


def user_stats(df):
    """Displays statistics on bikeshare users."""

    print('\nCalculating User Stats...\n')
    start_time = time.time()

    # TO DO: Display counts of user types
    usertypes = df['User Type'].value_counts()
    print('user types:\n', usertypes)

    # TO DO: Display counts of gender
    try:
      gendertypes = df['Gender'].value_counts()
      print('\n gender Types:\n', gendertypes)
    except KeyError:
      print("\n gender Types:\n no data available .")

    # TO DO: Display earliest, most recent, and most common year of birth
    try:
      EarliestYear = df['Birth Year'].min()
      print('\n earliest Year:', EarliestYear)
    except KeyError:
      print("\n earliest Year:\n no data available .")

    try:
      MostRecentYear = df['birth year'].max()
      print('\n most recent year:', MostRecentYear)
    except KeyError:
      print("\n most recent year:\n no data available .")

    try:
      MostCommonYear = df['birth year'].value_counts().idxmax()
      print('\n common year:', MostCommonYear)
    except KeyError:
      print("\n common year:\n no data available .")

    print("\nThis took %s seconds." % (time.time() - start_time))
    print('-'*40)


def main():
    while True:
        city, month, day = get_filters()
        df = load_data(city, month, day)

        time_stats(df)
        station_stats(df)
        trip_duration_stats(df)
        user_stats(df)

        restart = input('\nWould you like to restart? Enter yes or no.\n')
        if restart.lower() != 'yes':
            break


if __name__ == "__main__":
	main()
