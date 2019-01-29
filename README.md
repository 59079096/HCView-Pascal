# HCView
一个类似word或wps用于文字排版相关功能的控件
HCView代码遵循BSD协议，你可以任意的用于商业项目和自由的项目中而不用通知我，使用时请注意BSD协议以下三点：
1.如果再发布的产品中包含源代码，则在源代码中必须带有原来代码中的BSD协议。
2.如果再发布的只是二进制类库/软件，则需要在类库/软件的文档和版权声明中包含原来代码中的BSD协议。
3.不可以用开源代码的作者/机构名字和原来产品的名字做市场推广。
你可以加入QQ群 649023932 来获取更多的技术交流。
 
demo编译步骤：
1.在delphi中的菜单Tools-Options对话框里选中Library节点，右侧Library path添加HCView源码的source路径；
2.打开Demo目录下的代码，编译，运行；
3.Delphi2010以上的版本编译如果提示找不到Delphi自带的单元，则在工程属性中（Project-Options-Delphi Compiler右侧Unit scope names）根据实际情况添加命名空间域
System.Win;Data.Win;Datasnap.Win;Web.Win;Soap.Win;Xml.Win;Bde;Vcl;Vcl.Imaging;Vcl.Touch;Vcl.Samples;Vcl.Shell;System;Xml;Data;Datasnap;Web;Soap;Winapi


HCView的代码在不断的完善中，目前它仍有一些不足，如果下列的不足是您有特定需求的，您可直接联系我，我将优先处理。
1.目前单段同一样式连续纯文本在100万个字符时的排版速度不如word，但并非达不和word一样的速度；
2.目前有添加、删除批注的功能，动态修改批注里的内容目前只完成了常见的删除文本和输入文本；
3.未实现批注的直接修改和删除，需要通过界面的菜单处理；
4.查找替换对话框中的全部替换功能未完成；
5.形状目前只完成了直线；
6.有些操作还未实现撤销/恢复的功能；
7.表格复制后粘贴到word中时未能保持表格格式；