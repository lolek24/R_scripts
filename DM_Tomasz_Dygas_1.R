# Tytuł: "Odkrywanie reguł asocjacyjnych w zbiorze Groceries"
# autor: "Tomasz Dygas"
# Cel eksperymentów: wybór reguł asocjacyjnych za pomocą algorytmu Apriori w zbiorze Groceries
# z koszyka grupy produktów mlecznych (dairy produts)
 

# wczytanie bibliotek
library(arules) # pakiet dla reguł asocjacycjnych
library(arulesViz) # wizualizacjia danych dla reguł asocjacyjnych
library(ggplot2)
library(reshape2) # pakiet wykorzystywany do konwersji danych z matrix na data frame

# wczytanie danych
data("Groceries")

# sprawdzenie danych 
summary(Groceries)
Groceries@itemInfo
itemFrequencyPlot(Groceries, topN = 20)
image(head(Groceries,300))

#Agregacja do poziomu 2
GroceriesLevel2 <- aggregate(Groceries, itemInfo(Groceries)[["level2"]])

# wykorzystanie funkcji crossTable z pakietu arules w celu wyznaczenia połączeń między parami grup produktów
GroceriesCrossTable <- crossTable(GroceriesLevel2, sort=TRUE)
GroceriesCrossTableSupport <- crossTable(GroceriesLevel2, measure="support", sort=TRUE)
GroceriesCrossTableLift <- crossTable(GroceriesLevel2, measure="lift", sort=TRUE)
GroceriesCrossTablechiSquared <- crossTable(GroceriesLevel2, measure="chiSquared", sort=TRUE)

melted_Groceries <- melt(GroceriesCrossTable, na.rm = TRUE)
# Heatmap 
ggplot(data = melted_Groceries, aes(Var2, Var1, fill = value))+
  geom_tile(aes(fill = value), color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "blue", 
                       name="ilość") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

# użycie algorytmu Eclat z pakietu arules w celu wyszukania najczęściej występujących produktów
frequentItems <- eclat(Groceries)
inspect(frequentItems)
# mleko pełne 25,6 procent wystepowania w zbiorze danych 


# uruchomienie funkcji apriori z ustawieniami początkowymi
rules <- apriori(Groceries)
# dla dużych zbiorów danych mamy bardzo wiele reguł
# musimy zdecydować jaki poziom wsparcia i pewności nas bedzie interesował 

# dane pomocnicze lista reguł w formie df
rules_df <- as(rules, "data.frame")
str(rules_df)

# ustalenie poziomu wsparcia i pewności, w celu wyboru bardziej interesujących reguł
rules <- apriori(Groceries, parameter = list(support = 0.006, confidence = 0.3, minlen = 2, maxlen = 2))
# zaokrąglenie wartości do 3 miejsc po przecinku
quality(rules) <- round(quality(rules), digits=3)


summary(rules)
inspect(head(rules))
# dostaliśmy informację jakie zostały wybrane reguły
# dla 9835 transakcji, przy wsparciu 0,006 i pewności 0,3
# np piąta reguła mówi że: jeśli klient kupi zioła to kupi też pełne mleko 
# Przy wsparciu 0,008 i pewności 0,47 możemy stwierdzić, że ta zasada obejmuje 
# 0,8 procent transakcji i jest prawidłowa w 47 procentach zakupów dotyczących ziół
# Wartość lift mówi nam, o ile bardziej prawdopodobne jest, że klient kupi mleko pełne w stosunku 
# do przeciętnego klienta, biorąc pod uwagę, że kupił on zioła. 
# Ponieważ wiemy, że około 25,6 procent klientów kupiło mleko pełne (wsparcie), 
# podczas gdy 47 procent klientów kupujących zioła kupiło mleko pełne (zaufanie),
# możemy obliczyć wartość podnoszenia jako 0,475 / 0,256 = 1,859, co odpowiada pokazanej wartości.
# 

# wybór reguł z koszyka produktów "dairy produce"
dairy.rules <- sort(subset(rules, lhs@itemInfo[["level2"]] %in% "dairy produce" | rhs@itemInfo[["level2"]] %in% "dairy produce"), by = "lift") 
inspect(dairy.rules) 
plot(dairy.rules, method = 'grouped', max = 25)
plot(dairy.rules,measure = c("support","lift"),shading = "confidence",jitter = 2)
plot(dairy.rules, method = "paracoord", control = list(reorder = TRUE))
plot(dairy.rules, method = 'graph', engine = "htmlwidget")

# sprawdzenie wybranych reguł 
inspect(dairy.rules, by = "lift")
#Wnioski
# otrzymaliśmy 8 reguł dla których lift jest większy od 1 to znaczy że istnieje zależność między kolumną lhs i rhs
# więc ludzie którzy kupują krojony ser (sliced cheese) są 2,3 raza bardziej skłoni do zakupu jogurtu i jest prawidłowe dla 32% transakcji dla krojonego sera
# 0,8% ogółu transakcji. Podobnie dla produktów butter milk. 
# dbając o dobro i czas klienta, można produkty tego typu ustawić w jednym miejscu lub blisko siebie. Dla sprzedaży internetowej np. przy zakupie maślanki(butter milk)
# lub krojonego sera można od razu proponować zakup jogurtu.


