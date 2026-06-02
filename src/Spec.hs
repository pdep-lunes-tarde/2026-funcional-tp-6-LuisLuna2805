module Spec where
import PdePreludat
import Library
import Test.Hspec
import Control.Exception (evaluate)

correrTests :: IO ()
correrTests = hspec $ do
    describe "TP 5" $ do
        describe "Parte 1" $ do
            it "precio final de cuartoDeLibra deberia ser 54" $ do
                precioFinal cuartoDeLibra `shouldBe` 54
            it "precio final de pdepBurger deberia ser 91.2" $ do
                precioFinal pdepBurger `shouldBe` 91.2
            it "agrandar hamburguesa deberia sumarle un medallon del que tenga" $ do
                agrandar cuartoDeLibra `shouldBe` Hamburguesa {precioBase = 20, ingredientes = [Pan,Carne,Cheddar,Pan,Carne]}
            it "agrandar varias veces hamburguesa deberia sumar tantos medaloones como se indique" $ do
                agrandarMultiple 3 cuartoDeLibra `shouldBe` Hamburguesa {precioBase = 20, ingredientes = [Pan,Carne,Cheddar,Pan,Carne,Carne,Carne]}
            it "agregar ingrediente deberia sumar un ingrediente de cualquier tipo " $ do
                agregarIngrediente Panceta cuartoDeLibra `shouldBe` Hamburguesa {precioBase = 20, ingredientes = [Pan,Carne,Cheddar,Pan,Panceta]}
            it "agregar ingredientes Multiples deberia sumar tantos ingredientes como se indique de cualquier tipo " $ do
                agregarIngredientes [Panceta, Cheddar, Pollo, Curry] cuartoDeLibra `shouldBe` Hamburguesa {precioBase = 20, ingredientes = [Pan,Carne,Cheddar,Pan,Panceta, Cheddar, Pollo, Curry]}
            it "aplicar descuento deberia realizar un descuento sobre el precioFinal" $ do
                descuento 50 cuartoDeLibra `shouldBe` 27

        describe "Parte 2" $ do
            it "ingredientes dobleCuarto deberia mostrar la composicion de la hamburguesa" $ do
                ingredientes dobleCuarto `shouldBe` [Pan,Carne,Cheddar,Pan,Cheddar,Carne]
            it "precio dobleCuarto deberia ser 84" $ do
                precioFinal dobleCuarto `shouldBe` 84
            it "bigPdep deberia mostrar la composicion de la hamburguesa" $ do
                ingredientes bigPdep `shouldBe` [Pan,Carne,Cheddar,Pan,Cheddar,Carne,Curry]
            it "precio bigPdep deberia ser 89" $ do
                precioFinal bigPdep `shouldBe` 89
            it "ingredientes delDia bigPdep deberia mostrar la composicion del combo" $ do
                ingredientes (delDia bigPdep) `shouldBe` [Pan,Carne,Cheddar,Pan,Cheddar,Carne,Curry,Papas]
            it "precio delDia bigPdep deberia ser 69.3 porque tiene 30% off" $ do
                precioFinal (delDia bigPdep) `shouldBe` 69.3
        
        describe "Parte 3" $ do
            it "hacer veggie dobleCuarto deberia volver las partes del combo veganas" $ do
                hacerVeggie dobleCuarto `shouldBe` Hamburguesa {precioBase = 20, ingredientes = [Pan,Pan,PatyVegano,PatyVegano,QuesoDeAlmendras,QuesoDeAlmendras]}
            it "hacer veggie bigPdep deberia volver las partes del combo veganas" $ do
                hacerVeggie bigPdep `shouldBe` Hamburguesa {precioBase = 20, ingredientes = [Pan,Pan,Curry,PatyVegano,PatyVegano,QuesoDeAlmendras,QuesoDeAlmendras]}
            it "hacer veggie dobleCuartoVegano deberia quedarse igual" $ do
                hacerVeggie dobleCuartoVegano `shouldBe` dobleCuartoVegano
            it "cambiar pan de paty deberia cambiar el pan por integral" $ do
                cambiarPanDePaty bigPdep `shouldBe` Hamburguesa {precioBase = 20, ingredientes = [Carne,Cheddar,Cheddar,Carne,Curry,PanIntegral,PanIntegral]}
            it "ingredientes dobleCuartoVegano deberia mostrar la composicion de la hamburguesa" $ do
                ingredientes dobleCuartoVegano `shouldBe` [PatyVegano,PatyVegano,QuesoDeAlmendras,QuesoDeAlmendras,PanIntegral,PanIntegral]
            it "precio dobleCuartoVegano deberia ser 76" $ do
                precioFinal dobleCuartoVegano `shouldBe` 76