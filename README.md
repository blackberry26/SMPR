- [Метрические алгоритмы классификации](https://github.com/blackberry26/SMPR#Метрические-алгоритмы-классификации)
  - [Метод ближайшего соседа (1NN)](https://github.com/blackberry26/SMPR#Метод-ближайшего-соседа-1nn)
  - [Метод k ближайших соседей](https://github.com/blackberry26/SMPR#Метод-k-ближайших-соседейknn)
  - [Метод k взвешенных ближайших соседей](https://github.com/blackberry26/SMPR#Метод-k-взвешенных-ближайших-соседей-kwnn)
  - [Метод Парзеновского окна](https://github.com/blackberry26/SMPR#Метод-Парзеновского-окна-pw)
  - [Метод потенциальных функций]()
# Метрические алгоритмы классификации

**Гипотеза компактности.** Схожим объектам соответствуют схожие ответы.

Для формализации понятия сходства вводится функция расстояния в пространстве объектов **Х**.


Имеется пространство объектов ![1111](https://user-images.githubusercontent.com/43415122/46093675-a1f6fc80-c1c0-11e8-8492-d1212656c198.png) и конечное множество имён классов ![11111](https://user-images.githubusercontent.com/43415122/46093711-b935ea00-c1c0-11e8-90bb-9a4716f8164c.png)
.
На множестве ![1111](https://user-images.githubusercontent.com/43415122/46093675-a1f6fc80-c1c0-11e8-8492-d1212656c198.png) задана функция расстояния ![11](https://user-images.githubusercontent.com/43415122/46093571-57758000-c1c0-11e8-92de-21dbb32a0a23.png)
Существует целевая зависимость ![111](https://user-images.githubusercontent.com/43415122/46093631-812ea700-c1c0-11e8-9372-60fb9c754406.png), значения которой известны только на объектах обучающей выборки![1](https://user-images.githubusercontent.com/43415122/46093396-e33adc80-c1bf-11e8-8f8d-179c7576fc9a.png)
 Требуется построить алгоритм классификации ![111111](https://user-images.githubusercontent.com/43415122/46093758-db2f6c80-c1c0-11e8-872b-ce1fc27e56f8.png) , аппроксимирующий целевую зависимость ![1111111](https://user-images.githubusercontent.com/43415122/46093786-f26e5a00-c1c0-11e8-8acb-3f1e34f336c1.png) на всём множестве ![1111](https://user-images.githubusercontent.com/43415122/46093675-a1f6fc80-c1c0-11e8-8492-d1212656c198.png).
 
 Метрические алгоритмы классификации основаны на анализе сходства объектов с помощью функции расстояния, т.е. чем меньше расстояние, тем больше объекты похожи друг на друга.
 
 # Метод ближайшего соседа (1NN)
 *Алгоритм ближайшего соседа* - самый простой алгоритм классификации. Он относит классифицируемый объект ![11111111](https://user-images.githubusercontent.com/43415122/46094409-8856b480-c1c2-11e8-9dd0-3ceefbc706df.png) к тому
классу, которому принадлежит ближайший обучающий объект:
![111111111](https://user-images.githubusercontent.com/43415122/46094442-a8867380-c1c2-11e8-8c37-5641da90f94a.png)

**Достоинства:**
- простота реализации

**Недостатки:**
- неустойчивость к погрешностям
- отсутствие параметров, которые можно было бы настраивать по выборке.


![1NN](https://user-images.githubusercontent.com/43415122/46460406-2565ae80-c7bc-11e8-8388-b0603bbddc08.png)



[Code](https://github.com/blackberry26/SMPR/blob/master/on%D1%82%D1%82e.R)

 # Метод k ближайших соседей (KNN)
В предыдущем методе учитывался только один сосед,теперь рассмотрим алгоритм, который использует *k* ближайших соседей.  
**KNN** - метрический алгоритм классификации, где в качестве оценки близости объекта ![1](https://user-images.githubusercontent.com/43415122/46746639-231dbb80-ccb8-11e8-8c33-c0a871926339.png) к классу ![2](https://user-images.githubusercontent.com/43415122/46746682-36c92200-ccb8-11e8-965b-d35e244a31e6.png) выступает функция-индикатор ![3](https://user-images.githubusercontent.com/43415122/46746693-4183b700-ccb8-11e8-8cba-76d363c8cb66.png) где ![4](https://user-images.githubusercontent.com/43415122/46746713-4f393c80-ccb8-11e8-988c-bcaa48ceaefc.png) — порядок ближайшего соседа к классифицируемой точке ![1](https://user-images.githubusercontent.com/43415122/46746639-231dbb80-ccb8-11e8-8c33-c0a871926339.png).

Проще говоря, алгоритм выбирает **k** ближайших соседей и возвращает тот класс, который среди выбранных встречается большее количество раз.
Для классификации используется стандартная выборка ирисов Фишера по лепесткам.

**Достоинства:**
- прост в реализации
- неплохие результаты при правильно подобраном k

**Недостатки:**
- необходимо хранить всю выборку целиком
- бедный набор параметров
- не все точки с одинаковым расстоянием будут учитаны
- примитивная оценка близости
- в случае одинаковых весов классов алгоритм выбирает любой



![47304893-94eaf300-d630-11e8-877f-7b6791c5117d](https://user-images.githubusercontent.com/43415122/47796964-72469180-dd36-11e8-9f30-b840f4d35373.png)

[code](https://github.com/blackberry26/SMPR/blob/master/knn.R)

**LOO(leave-one-out) для Knn**
**LOO**(оценка скользящего контроля) - проверка, которая необходима, чтобы оценить оптимальность алгоритма(выявить при каких *k* оптимален ) и на сколько он ошибается.

Нужно проверить, как часто будет ошибаться алгоритм, если по одному выбирать элементы из обучающей выборки.
Алгоритм состоит в следующем: извлечь элемент, обучить оставшиеся элементы, классифицировать извлеченный, затем вернуть его обратно. Так нужно поступить со всеми элементами выборки.

**График зависимости LOO(при k=6)**

![default](https://user-images.githubusercontent.com/43415122/47837941-72867180-ddb6-11e8-839a-6e8e56a779b5.png) ![photofacefun_com_1541056584](https://user-images.githubusercontent.com/43415122/47838076-e32d8e00-ddb6-11e8-8cdd-cc902cf61209.jpg)

[code](https://github.com/blackberry26/SMPR/blob/master/knnn.R)


# Метод k взвешенных ближайших соседей (KWNN)
Метод **K взвешенных ближайших соседей** - это метрический алгоритм классификации, основанный на оценивании сходства объектов. Классифицируемый объект относится к тому классу, которому принадлежат ближайшие к нему объекты обучающей выборки. 

 В качестве оценки близости объекта **u** к классу **Y** выступает функция ![yeyer](https://user-images.githubusercontent.com/43415122/47447139-897d0080-d7c5-11e8-96a8-fd9a270deeef.png) где
  - **i** -порядок ближайшего соседа к классифицируемой точке **u**; 
  - **w(i)** — вес относительно точки **u**, строго убывающая функция.
  
  В качестве весовой функции возьмем нелинейную последовательность, например геометрическую прогрессию:![photofacefun_com_1541660607](https://user-images.githubusercontent.com/43415122/48182794-3d899a00-e335-11e8-9877-f0e9cefcf164.jpg)
  
Возьмем для примера k=6(т.к. в LOO для KNN он наиболее оптимальный),получаем результат при k =6 и q = 1,который равен 0.3.


  
![kwnn](https://user-images.githubusercontent.com/43415122/48185256-1d5dd900-e33d-11e8-9f71-886602c28463.png)



 
 **Достоинства**

- простота реализации
- классификацию, проведенную данным алгоритмом, легко интерпретировать путём предъявления пользователю нескольких ближайших объектов

**Недостатки**

- неэффективный расход памяти 
- необходимость хранения обучающей выборки целиком
- поиск ближайшего соседа предполагает сравнение классифицируемого объекта со всеми объектами выборки, что требует линейного по длине выборки числа операций

[code](https://github.com/blackberry26/SMPR/blob/master/KWNN.R)

Из-за того, что алгоритм *k взвешенных ближайших соседей* учитывает порядок объектов при классификации, он выдаёт лучший результат, чем алгоритм k ближайших соседей (kNN). Следовательно, объекты **x_i**, которые находятся ближе к классифицируемому объекту **u**, будут оказывать намного большее влияние, чем те **x_i**, которые дальше (из-за учёта порядка объектов).

 # Метод парзеновского окна (PW)
 
 Рассмотрим весовую функцию **w(i,u)** как функцию не от ранга соседа, а от расстояния ![q4](https://user-images.githubusercontent.com/43415122/48507752-db152a00-e85d-11e8-8dd4-bc515803eaa6.png)
 
![q1](https://user-images.githubusercontent.com/43415122/48535492-396cf780-e8b4-11e8-8fc0-0ebebabbd198.png)
где K(z)-функция ядра невозрастающая на![2018-11-15 10-58-39_cut-photo ru](https://user-images.githubusercontent.com/43415122/48535817-33c3e180-e8b5-11e8-88e9-5fb35e2ca7aa.png)

В этом случае метрический классификатор имеет вид: ![q2](https://user-images.githubusercontent.com/43415122/48535883-6c63bb00-e8b5-11e8-9b9c-d17c24c716b3.png).

Алгоритм  ![q3](https://user-images.githubusercontent.com/43415122/48535899-7f768b00-e8b5-11e8-851f-ac48b89f783b.png)
называется алгоритмом парзеновского окна. 

Окно - сферическая окрестность объекта  *u*  радиуса  *h* , при попадании в который обучающий объект  **x_i**  "голосует" за отнесение объекта   *u *  к классу  **y_i**.

*h* -ширина окна,задается априори либо с помощью LOO(h).

Чаще всего применяются 5 типов ядер:

 - Прямоугольное ![n](https://user-images.githubusercontent.com/43415122/48536606-c2396280-e8b7-11e8-8f66-6bccab278452.png)
 ![pw_png rect](https://user-images.githubusercontent.com/43415122/48887439-ece76600-ee37-11e8-9862-d417e3cab634.png)

 - Квартическое ![12](https://user-images.githubusercontent.com/43415122/48536623-cfeee800-e8b7-11e8-9214-e253600fb9c5.png)
 ![pw_png quart](https://user-images.githubusercontent.com/43415122/48887459-f83a9180-ee37-11e8-9115-6e975c2bbf86.png)

 - Треугольное  ![1](https://user-images.githubusercontent.com/43415122/48536643-dd0bd700-e8b7-11e8-9e4d-df0d859c310f.png) ![pw_png triangle](https://user-images.githubusercontent.com/43415122/48887418-df31e080-ee37-11e8-81fd-3e3dae810af1.png)

 - Епанечникова ![123](https://user-images.githubusercontent.com/43415122/48536659-e72dd580-e8b7-11e8-9046-8053c209ddf3.png)
 ![pw_png ep](https://user-images.githubusercontent.com/43415122/48887467-025c9000-ee38-11e8-83fb-e5a6abb25eb6.png)
 - Гауссовское (нормальное распределение) ![pw_png gaussian](https://user-images.githubusercontent.com/43415122/48887354-a98cf780-ee37-11e8-8978-e4e4ff1e3bc9.png)
 
  **Вывод**: Среди всех классифицированных ядер больше всего выделяется гауссовское ядро. Оно однозначно разделило классы на всей плоскости.
  
 [Code](https://github.com/blackberry26/SMPR/blob/master/PW.R)





**Достоинства**
   
 - Простота реализации.
 - Все точки, попадающие в окно, расстояние между которыми одинаково, будут учитываться (в отличие от kWNN).
 - Скорость классификации быстрее, т.к. не требуется сортировка расстояний (O(l)).
 - Окно с переменной шириной решает проблему разрешимости задач, в которых обучающие выборки распределены неравномерно по пространству X.
  

**Недостатки**

 - "Бедный" набор параметров.
 - Если суммарные веса классов оказываются одинаковыми, то алгоритм относит классифицируемый объект u к любому из классов.
 - Слишком узкие окна приводят к неустойчивой классификации, а слишком широкие - к вырождению алгоритма в константу.
 - Диапазон, из которого выбирается параметр h, нужно подбирать самим.
 - Если в окно, радиуса h, не попало ни одной точки x_i, то алгоритм не способен классифицировать объект u.
 
 
 # Метод потенциальных функций (PF)
Если в методе парзеновского окна центр окна поместить в обучающие объекты, то получим метод потенциальных функций.
Он позволяет с помощью простого алгоритма оценивать вес («важность») объектов обучающей выборки при решении задачи классификации.
Пусть имеется пространство объектов X и конечное множество классов Y. На множестве X задана функция расстояния \rho: X \times X \to [0, + \infty]. Каждый объект из X относится к некоторому классу из Y посредством отображения y^*:~X \to Y, 


 
 
