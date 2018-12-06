- [Метрические алгоритмы классификации](https://github.com/blackberry26/SMPR#Метрические-алгоритмы-классификации)
  - [Метод ближайшего соседа (1NN)](https://github.com/blackberry26/SMPR#Метод-ближайшего-соседа-1nn)
  - [Метод k ближайших соседей](https://github.com/blackberry26/SMPR#Метод-k-ближайших-соседейknn)
  - [Метод k взвешенных ближайших соседей](https://github.com/blackberry26/SMPR#Метод-k-взвешенных-ближайших-соседей-kwnn)
  - [Метод Парзеновского окна](https://github.com/blackberry26/SMPR#Метод-Парзеновского-окна-pw)
  - [Метод потенциальных функций](https://github.com/blackberry26/SMPR#Метод-потенциальных-функций-pf)
  - [Сравнение алгоритмов классификации](https://github.com/blackberry26/SMPR#Сравнение-алгоритмов-классификации)
- [Байесовские классификаторы](https://github.com/blackberry26/SMPR#Байесовские-классификаторы)

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

 - Прямоугольное ![68747470733a2f2f6c617465782e636f6465636f67732e636f6d2f6769662e6c617465783f522872293d253543667261632537423125374425374232253744253542253743722537432535436c65712673706163653b31253544](https://user-images.githubusercontent.com/43415122/49207845-c9d02f80-f3be-11e8-8b82-d5d57b627f2b.gif)

 ![default](https://user-images.githubusercontent.com/43415122/49180805-cb6e0900-f366-11e8-80af-6e4b1c8d5ea7.png)


 - Квартическое ![rtrt](https://user-images.githubusercontent.com/43415122/49207928-08fe8080-f3bf-11e8-993e-3c4f43ba46dc.gif)


 - Треугольное![ju](https://user-images.githubusercontent.com/43415122/49207873-deacc300-f3be-11e8-9a3b-67513a09f0aa.gif)

 ![default](https://user-images.githubusercontent.com/43415122/49180500-eb50fd00-f365-11e8-88b8-c44ed06dfc0d.png)

 - Епанечникова ![i](https://user-images.githubusercontent.com/43415122/49207889-ee2c0c00-f3be-11e8-9696-b57211d05f28.gif) ![default](https://user-images.githubusercontent.com/43415122/49180695-7500ca80-f366-11e8-971a-6d97799f09ad.png)
 - Гауссовское (нормальное распределение) ![default](https://user-images.githubusercontent.com/43415122/49180753-a4afd280-f366-11e8-9b18-d7154928e105.png)
 
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
 
Если в методе парзеновского окна центр окна поместить в обучающие объекты, то получим *метод потенциальных функций.*
Он позволяет с помощью простого алгоритма оценивать вес («важность») объектов обучающей выборки при решении задачи классификации.
Пусть имеется пространство объектов X и конечное множество классов Y. 
На множестве **X** задана функция расстояния ![default](https://user-images.githubusercontent.com/43415122/49168353-57bd0380-f348-11e8-9c6a-51e763adecba.png).

Каждый объект из **X** относится к некоторому классу из **Y** посредством отображения ![default](https://user-images.githubusercontent.com/43415122/49168416-7d4a0d00-f348-11e8-909f-9441410cba17.png).
Пусть также задана обучающая выборка пар «объект—ответ»: ![default](https://user-images.githubusercontent.com/43415122/49168482-a074bc80-f348-11e8-963e-77c4765edb10.png).

Требуется построить алгоритм  ![default](https://user-images.githubusercontent.com/43415122/49168564-ce5a0100-f348-11e8-86f7-f7049cdbdc50.png), который по заданной выборке  ![default](https://user-images.githubusercontent.com/43415122/49168592-e0d43a80-f348-11e8-8c51-286156b22608.png) аппроксимирует отображение ![default](https://user-images.githubusercontent.com/43415122/49168627-f9dceb80-f348-11e8-86a3-4886eb92ce44.png).

 **Алгоритм**
 
*Вход*:Обучающая выборка из l пар «объект-ответ» ![default](https://user-images.githubusercontent.com/43415122/49168732-36a8e280-f349-11e8-8ce2-c8332bb410d4.png)

*Выход*: Значения параметров ![default](https://user-images.githubusercontent.com/43415122/49168800-5fc97300-f349-11e8-9b9b-561b29388fd6.png) для всех ![default](https://user-images.githubusercontent.com/43415122/49168978-cd759f00-f349-11e8-8874-b16173bdc53b.png).

*Описание алгоритма*:
1. Инициализация: ![default](https://user-images.githubusercontent.com/43415122/49169037-f85ff300-f349-11e8-8c91-bb32fb2ab2b7.png)для всех ![default](https://user-images.githubusercontent.com/43415122/49169069-0ada2c80-f34a-11e8-88f0-034964a62a80.png).
2. Повторять пункты 3-4, пока ![default](https://user-images.githubusercontent.com/43415122/49169127-2cd3af00-f34a-11e8-90c3-8d41a5fa8635.png)(то есть пока процесс не стабилизируется):
 3.  Выбрать очередной объект x_i из выборки;
 4.  Если ![default](https://user-images.githubusercontent.com/43415122/49169229-673d4c00-f34a-11e8-8fc8-7e2ce002a164.png), то ![default](https://user-images.githubusercontent.com/43415122/49169291-86d47480-f34a-11e8-8764-d6ccc3b0d41a.png).
 
 5.  Вернуть значение ![default](https://user-images.githubusercontent.com/43415122/49169353-a23f7f80-f34a-11e8-90ba-523f6134ee1b.png) для всех ![default](https://user-images.githubusercontent.com/43415122/49169402-b8e5d680-f34a-11e8-801e-4af1469ab30a.png).
 
 6.  Пока число ошибок на выборке не окажется достаточно мало.

**Достоинства**:
 - Метод прост для понимания и алгоритмической реализации;
 - Порождает потоковый алгоритм;
 - Хранит лишь часть выборки, следовательно, экономит память.
 
 **Недостатки**:
 - Порождаемый алгоритм медленно сходится
 
 
 # Сравнение алгоритмов классификации

 <table>
  <tr>
    <th>Метод</th>
    <th>Параметры</th>
    <th>Величина ошибок</th>
  </tr>
  <tr>
    <td>KNN</td>
    <td>k=6</td>
    <td>0.033</td>
  </tr>
  <tr>
    <td>KWNN</td>
    <td>k=9</td>
    <td>0.033</td>
  </tr>
  <tr>
    <td>PW,Гауссовское ядро</td>
    <td>h=0.1</td>
    <td>0.04</td>
  </tr>
  <td>PW,ядро Епанечникова</td>
    <td>h=0.4</td>
    <td>0.04</td>
  </tr>
  </tr>
  <td>PW,Треугольное ядро</td>
    <td>h=0.4</td>
    <td>0.04</td>
  </tr>
  </tr>
  <td>PW,Квартическое ядро</td>
    <td>h=0.4</td>
    <td>0.04</td>
  </tr>
  </tr>
  <td>PW,Прямоугольное ядро</td>
    <td>h=0.4</td>
    <td>0.04</td>
  </tr>
</table>
 
 
 
 # Байесовские классификаторы
 
 Байесовские алгоритмы классификации основаны на предположении, что есть вероятностное пространство X x Y с неизвестной плотностью распределения ![68747470733a2f2f6c617465782e636f6465636f67732e636f6d2f6769662e6c617465783f25354372686f2532302532387825324325323079253239253230253344253230502532387925323925354372686f2532302532387825323025374325323079253239](https://user-images.githubusercontent.com/43415122/49568481-c3e7ca80-f939-11e8-86a8-6ec878a71f67.gif)
, из которого случайно и независимо извлекаются l наблюдений.

Байесовский подход опирается на теорему о том, что *если плотности распределения классов известны, то алгоритм классификации, имеющий минимальную вероятность ошибок, можно выписать в явном виде.*

Обозначим величину потери алгоритмом **а** при неправильной классификации объекта класса **y** .

**Теорема**: Если известны априорные вероятности классов P(y) и функции правдоподобия p(x|y), то минимум среднего риска достигается алгоритмом ![yutyg](https://user-images.githubusercontent.com/43415122/49568554-fc87a400-f939-11e8-8726-9ec6aafe5eab.gif)
 Алгоритм **a(x)** называется оптимальным *байесовским решающим правилом.*

На практике зачастую плотности распределения классов неизвестны и их приходится восстанавливать по обучающей выборке. *Чем лучше удастся восстановить функции правдоподобия, тем ближе к оптимальному будет построенный алгоритм.*

В зависимости от способов восстановления плотности существует большое разнообразие байесовских алгоритмов классификации.

# Линии уровня нормального распределения

Это специальный случай баесовской классификации, когда предполагается, что плотности всех классов являются многомерными нормальными. В этом случае задача решается аналитически. Сами плотности вычисляются по формуле:
![2018-12-06 11-59-40_cut-photo ru](https://user-images.githubusercontent.com/43415122/49570479-5e96d800-f93f-11e8-83b7-481fd33d2db5.png)
в которой 
X-объект, состоящий из n признаков,
 \mu  -математическое ожидание,
 \Sigma  -ковариационная матрица.

**Примеры для таких центров и ковариационных матриц:**


 **1)mu = {0, 0}, cov = {1,0,0,1}**
 
![photofacefun_com_1544082445](https://user-images.githubusercontent.com/43415122/49569212-feeafd80-f93b-11e8-97c3-edaedebffee7.jpg)

 **2)mu = {0, 0}, cov = {1,1,0,1}**
 
![photofacefun_com_1544082939](https://user-images.githubusercontent.com/43415122/49569622-1c6c9700-f93d-11e8-908a-e9190decbcc9.jpg)

**3)mu = {0, 0}, cov = {3,0,0,1}**

![photofacefun_com_1544082684](https://user-images.githubusercontent.com/43415122/49569411-89336180-f93c-11e8-98d1-da7b27903020.jpg)

**4)mu = {0, 0}, cov = {1,0,0,3}**

![photofacefun_com_1544082826](https://user-images.githubusercontent.com/43415122/49569524-d9aabf00-f93c-11e8-85f9-17f491115cb6.jpg)



