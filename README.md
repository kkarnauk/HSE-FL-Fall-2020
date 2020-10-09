## Regexp Recursive Descent Parser

### Установка
Парсер написан на C#. Чтобы уметь запускать это под Ubuntu, нужно иметь dotnet. Для этого можно последовать инструкции [отсюда](https://docs.microsoft.com/ru-ru/dotnet/core/install/linux-debian).

### Использование
Чтобы запустить саму программу, нужно написать в консоли `dotnet run` и написать регулярное выражение (формат обозначен ниже). Если хотите запустить тесты, то `dotnet run tests`.

### Формат  
* `Seq first_expr second_expr` — конкатенация выражений.
* `Alt first_expr second_expr` — дизъюнкция.
* `Star expr` — звезда Клини.
* `'identifier'` — идентификатор.
* `Epsilon` — пустое слово.
* `Empty` — пустое множество.
