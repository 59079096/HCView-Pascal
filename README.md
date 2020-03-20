﻿# HCView
一个类似word或wps用于文字排版相关功能的控件，有Delphi、C#、Html5、C++(暂未完成)四个版本。

HCView代码遵循BSD协议，你可以任意的用于商业项目和自由的项目中而不用通知我，使用时请注意BSD协议以下三点：

1.如果再发布的产品中包含源代码，则在源代码中必须带有原来代码中的BSD协议。

2.如果再发布的只是二进制类库/软件，则需要在类库/软件的文档和版权声明中包含原来代码中的BSD协议。

3.不可以用开源代码的作者/机构名字和原来产品的名字做市场推广。

你可以加入QQ群 649023932 来获取更多的技术交流。


![图片说明](https://github.com/59079096/HCView-Pascal/blob/master/demo/HCView/HCViewDemo.png)
 
demo编译步骤：
1.在delphi中的菜单Tools-Options对话框里选中Library节点，右侧Library path添加HCView源码的source路径；
2.打开Demo目录下的代码，编译，运行；
3.Delphi2010以上的版本编译如果提示找不到Delphi自带的单元，则在工程属性中（Project-Options-Delphi Compiler右侧Unit scope names）根据实际情况添加命名空间域
System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi


友情提示：
1.HCView默认支持藏文等有紧缩字符的文本，这导致文件保存后体积比不支持藏文等要大，如果你只需要中文和英文支持且对体积有敏感，可以关闭全局的条件编译符号 UNPLACEHOLDERCHAR，关闭后文件保存体积较关闭前理论上会减小约50%，注意关闭后打开关闭前保存的文档会不正常。