using Avalonia.Data.Converters;
using System;
using System.Globalization;

namespace Transmute.Desktop
{
    /// <summary>
    /// Calculates maximum width of the description column in the navigation list. The ratio corresponds to the grid column definition in RulesPane.axaml.
    /// </summary>
    public class NavigationListCellWidthConverter : IValueConverter
    {
        public object? Convert(object? value, Type targetType, object? parameter, CultureInfo culture)
        {
            if (value is not double width)
            {
                return value;
            }

            return width * 10 / 16;
        }

        public object? ConvertBack(object? value, Type targetType, object? parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }
}
