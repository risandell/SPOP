module Przypomnienia where
{-
Projekt z przedmiotu SPOP - Przypomnienia 
Katarzyna Kucharczyk
Pawe³ Matuszewski
-}
import Model
import Data.Time hiding (Day)
-- ****
-- main
-- ****
main :: IO ()
main = do
    menu(empty, empty) -- obie listy na poczatku puste
-- ***********
-- menu g³ówne
-- przyjmuje dwa parametry (liste zaplanowanych i liste zrealizowanych)
-- ***********
menu (LZap lzap, LZre lzre)= do
    putStrLn "*****Program - Przypomnienia*****"
    putStrLn "***Mozliwe operacje:"
    putStrLn "*1. Zarzadzanie lista zaplanowanych zadan"
    putStrLn "*2. Zarzadzanie lista zrealizowanych zadan"
    putStrLn "*3. Zapis/odczyt zaplanowanego/zrealizowanego zadania"
    putStrLn "*4. Podanie daty i godziny (test)"
    putStrLn "*5. Zakoncz"
    putStrLn "*********************************"
    option <- getLine
    case option of
        "1" -> zarzadzanieZaplanowanymi(LZap lzap, LZre lzre)
        "2" -> zarzadzanieZrealizowanymi(LZap lzap, LZre lzre)
        "3" -> menuZapisOdczyt(LZap lzap, LZre lzre)
        "4" -> putStrLn "Dzisiejsza data to:"
        "5" -> return()
        otherwise -> do
            putStrLn "Zla opcja!"
            menu (LZap lzap, LZre lzre)
-- ************************************
-- menu zarz¹dzania zaplanowanych zadañ
-- ************************************
zarzadzanieZaplanowanymi(LZap lzap, LZre lzre) = do
    putStrLn ""
    putStrLn "Zarzadzanie zaplanowanych zadan"
    putStrLn "Dostepne opcje:"
    putStrLn "1. Utworz nowe zadanie do wykonania"
    putStrLn "2. Wyswietl wszystkie zaplanowane zadania"
    putStrLn "3. Wyswietl zadania do zrealizowania w dniu dzisiejszym (w tym zalegle"
    putStrLn "4. Wybierz zadanie jako zrealizowane"
    putStrLn "5. Usun zadanie lub wszystkie zadania"
    putStrLn "6. Powrot"
    option <- getLine
    case option of
        "1" -> menuDodawania(LZap lzap, LZre lzre)
        "2" -> do 
            print (LZap lzap)
            zarzadzanieZaplanowanymi(LZap lzap, LZre lzre)
        "5" -> menuUsuwania(LZap lzap, LZre lzre, 0)
        "6" -> menu(LZap lzap, LZre lzre)
        otherwise -> do
            putStrLn "Zla opcja!"
            zarzadzanieZaplanowanymi(LZap lzap, LZre lzre)
-- **********************
-- menu dodawania zadania
-- **********************
menuDodawania(LZap lzap, LZre lzre) = do
    putStrLn ""
    putStrLn "Wprowadz nazwe zadania"
    nazwa <- getLine
    putStrLn "Wprowadz date zadania"
    putStrLn "Dzien: (Pon / Wt / Sr / Czw / Pt / Sob / Nie)"
    dzien <- getLine
    putStrLn "Miesiac: (Sty / Lut / Mar / Kwi / Maj / Cze / Lip / Sie / Wrz / Paz / Lis / Gru)"
    miesiac <- getLine
    putStrLn "Rok:"
    rok <- getLine
    putStrLn "Wprowadz godzine (hh:mm)"
    godzina <- getLine
    putStrLn "Wprowadz powtarzalnosc zdarzenia (jednorazowe/co dzien/co tydzien/co miesiac/co rok"
    okres <- getLine
    if (czyString nazwa == False) || (czyDzien dzien == False) || (czyMiesiac miesiac == False) || (czyLiczba rok == False) || (czyGodzina godzina == False)|| (czyPowtarzalnosc okres == False) then do
        putStrLn "Wprowadzono bledne dane!"
        putStrLn "Powrot do zarzadzania zaplanowanymi zadaniami"
        zarzadzanieZaplanowanymi(LZap lzap, LZre lzre)
	else do
        putStrLn "Stworzono obiekt"
        print (Zadanie nazwa (DataZadania (string2Dzien dzien) (string2Miesiac miesiac) (string2int rok)) (read(godzina ++ ":00")::TimeOfDay) (string2Powtarzalnosc okres))
        zarzadzanieZaplanowanymi((insert (Zadanie nazwa (DataZadania (string2Dzien dzien) (string2Miesiac miesiac) (string2int rok)) (read(godzina ++ ":00")::TimeOfDay) (string2Powtarzalnosc okres)) (LZap lzap)), LZre lzre)
-- *********************
-- menu usuwania zadañ
-- (lista zaplanowanych, lista zrealizowanych,tryb (0/1))
-- tryb = 0 - usuwanie zaplanowanych
-- tryb = 1 - usuwanie zrealizowanych
-- *********************
menuUsuwania(LZap lzap, LZre lzre,tryb) = do
    putStrLn ""
    if tryb == 0 then putStrLn "Usuwanie zadan zaplanowanych"
    else putStrLn "Usuwanie zadan zrealizowanych"
    putStrLn "1. Usun podane zadanie" -- TODO usuwanie po wybranej nazwie
    putStrLn "2. Usun wszystkie zadania z listy"
    putStrLn "3. Powrot do menu zarzadzania zadaniami"
    option <- getLine
    case option of
        "1" -> do
            putStrLn "Podaj nazwe"
            nazwa <- getLine
            if czyString nazwa == False then do
                putStrLn "Bledny ciag znakow\nSprobuj ponownie."
                menuUsuwania(LZap lzap, LZre lzre,tryb)
            else do
                putStrLn "Na razie nie zaimplementowane"
                menuUsuwania(LZap lzap, LZre lzre,tryb)
        "2" -> do
            if tryb == 0 then zarzadzanieZaplanowanymi(deleteAll (LZap lzap),LZre lzre)
            else zarzadzanieZrealizowanymi(LZap lzap, deleteAll (LZre lzre))
        "3" -> do
            if tryb == 0 then zarzadzanieZaplanowanymi(LZap lzap, LZre lzre)
            else zarzadzanieZrealizowanymi(LZap lzap, LZre lzre)
        otherwise -> do
            putStrLn "Zla opcja!"
            menuUsuwania(LZap lzap, LZre lzre,tryb)
-- *************************************
-- menu zarz¹dzania zrealizowanych zadañ
-- *************************************
zarzadzanieZrealizowanymi(LZap lzap, LZre lzre) = do
    putStrLn ""
    putStrLn "Zarzadzanie zrealizowanych zadan"
    putStrLn "Dostepne opcje:"
    putStrLn "1. Wyswietl zrealizowane zadania"
    putStrLn "2. Usun zrealizowane zadanie lub wszystkie zadania"
    putStrLn "3. Powrot"
    option <- getLine
    case option of
        "1" -> do
            print (LZre lzre)
            zarzadzanieZrealizowanymi(LZap lzap, LZre lzre)
        "2" -> menuUsuwania(LZap lzap, LZre lzre,1)
        "3" -> menu(LZap lzap, LZre lzre)
        otherwise -> do
            putStrLn "Zla opcja!"
            zarzadzanieZrealizowanymi(LZap lzap, LZre lzre)
-- *******************
-- menu odczytu/zapisu
-- *******************
menuZapisOdczyt(LZap lzap, LZre lzre) = do
    putStrLn ""
    putStrLn "Menu odczytu/zapisu zaplanowanych/zrealizowanych zadan"
    putStrLn "Dostepne opcje:"
    putStrLn "1. Zapisanie zaplanowanych zadan do pliku"
    putStrLn "2. Odczytanie zaplanowanych zadan z pliku"
    putStrLn "3. Zapisanie zrealizowanych zadan do pliku"
    putStrLn "4. Odczytanie zrealizowanych zadan z pliku"
    putStrLn "5. Powrot"
    option <- getLine
    case option of
        "5" -> menu(LZap lzap, LZre lzre)
        otherwise -> do
            putStrLn "Zla opcja!"
            menuZapisOdczyt(LZap lzap, LZre lzre)