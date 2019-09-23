*&---------------------------------------------------------------------*
*& Report  ZTEST_JWJ3
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT Zgobang.
TYPES c4(4) TYPE c.
"类定义

CLASS cl_gobang DEFINITION.
  PUBLIC SECTION.
    "SALV对象
    DATA: mo_qp TYPE REF TO cl_salv_table.
    "常量：边界，以及两种棋子的图标
    CONSTANTS:border_ins TYPE c4 VALUE icon_border_inside."border
    CONSTANTS:player_wht TYPE c4 VALUE icon_incomplete.
    CONSTANTS:player_bak TYPE c4 VALUE icon_dummy.
    "步数，当前设定人先落子，所以步数为单数，轮到AI （可动态设定）
    DATA:step TYPE p VALUE 0.
    "核心表，存储当前棋盘
    "核心表设计思路：
    "需要模拟数组进行读取操作，所以对于行列敏感，除去边界，1-15行以及1-15列为棋盘，X为内表行，Y为列，列递增寻址，所以要基于列名的可拼接
    " 如C101，即为棋盘内表的第2列，棋盘的第一列，所以读取方式为 it_qp[ X ]-|C {Y + 99}|  => C {Y + 99}即为列名
    " 以下当传入参数为行列的i类型时，Y坐标为列的数字-99
    DATA :BEGIN OF wa_qp,
            c100 TYPE c4, "border
            c101 TYPE c4,c102 TYPE c4,c103 TYPE c4,c104 TYPE c4,c105 TYPE c4,
            c106 TYPE c4,c107 TYPE c4,c108 TYPE c4,c109 TYPE c4,c110 TYPE c4,
            c111 TYPE c4,c112 TYPE c4,c113 TYPE c4,c114 TYPE c4,c115 TYPE c4,
            c116 TYPE c4, "border
            "该列为TYPE列，设定该列支持link事件，详见初始化函数实现
            ctyp TYPE salv_t_int4_column,
          END OF wa_qp.
    DATA:it_qp LIKE TABLE OF wa_qp.
    "初始化棋盘，17*17的内表 其中最外围为边界，所以棋盘大小为15*15
    METHODS: ini_gobang.
    "棋盘绘制，调用SALV模拟棋盘
    METHODS: dis_gobang.
    "落子算法，绑定SALV的link click事件
    METHODS: set_gobang FOR EVENT link_click OF cl_salv_events_table IMPORTING row column.
    "AI核心算法，人落子之后，循环棋盘上所有未落子的点，调用估值算法估值，选择估值最高的点落子（基于博弈树的估值算法）
    METHODS: ai1_gobang."
    "模拟数组，并基于偏移方向以及偏移量的内表读取，分8个方向
    "偏移方向以及偏移量是可选的，当不传入的时候，返回当前坐标的值
    "返回值为基于偏移方向以及偏移量的内表字段值
    METHODS: get_gobang IMPORTING row TYPE i col TYPE i off TYPE i OPTIONAL set TYPE i OPTIONAL "偏移量
                                                                       RETURNING VALUE(value) TYPE c4.
    "AI核心估值算法，传入该点坐标以及需要估值的玩家，返回该点的估值
    METHODS: env_gobang IMPORTING row TYPE i col TYPE i player TYPE c4 RETURNING VALUE(value) TYPE i.
    "判负算法，通过对该点8个方向的延伸探索，判断四条线的同类棋子数目
    "返回值为以该点为原点，一条线上连续棋子的数量
    METHODS: win_gobang IMPORTING row TYPE i off TYPE i column TYPE c4 RETURNING VALUE(score) TYPE i."判负算法
    "消息
    METHODS: mes_gobang IMPORTING mes TYPE string.
ENDCLASS.

"主进程 实例化类并调用显示即可
DATA(gobang) = NEW cl_gobang( ).
gobang->dis_gobang( ).

"类的实现部分
CLASS cl_gobang IMPLEMENTATION.

  "判负算法
  METHOD: win_gobang.
    score = 1.
    DATA(col_cur)  = CONV i( column+1(3) - 99 ).
    ASSIGN COMPONENT column OF STRUCTURE it_qp[ row ] TO FIELD-SYMBOL(<pawn>) ."获取当前棋子类型
    "以下两次循环，通过判断方向以及偏移量判定同种棋子的类型，
    "当偏移量为正负时，8个方向可以看成四条线，所以调用时判断四次即可，
    DO 4 TIMES.
      IF <pawn> NE get_gobang( row = row col = col_cur off = off set =  sy-index  ).EXIT.ENDIF."若该方向遇见不同棋子，直接结束循环，下同
      score = score + 1.
    ENDDO.
    "一条线的另一方向
    DO 4 TIMES.
      IF <pawn> NE get_gobang( row = row col = col_cur off = off set = - sy-index ).EXIT.ENDIF.
      score = score + 1.
    ENDDO.

  ENDMETHOD.

* 基于偏移向量以及偏移量的读取
  METHOD: get_gobang.

    DATA(row_now) = row.
    DATA(col_now) = col.
    CASE off.
      WHEN 1.row_now = row_now + set.                               "  方向图示 0为初始点
      WHEN 2.col_now = col_now + set.                               "                   7      2      8
      WHEN 3.row_now = row_now - set.                               "
      WHEN 4.col_now = col_now - set.                               "
      WHEN 5.row_now = row_now + set.col_now = col_now + set.       "                   4      0      3
      WHEN 6.row_now = row_now + set.col_now = col_now - set.       "
      WHEN 7.row_now = row_now - set.col_now = col_now - set.       "
      WHEN 8.row_now = row_now - set.col_now = col_now + set.       "                   6      1      5
      WHEN OTHERS.
    ENDCASE.

*    IF col_now < 1 OR col_now > 16 OR row_now < 1 OR row_now > 17.
*      value = border_ins.RETURN.
*    ENDIF.
    "返回基于方向以及偏移量的值，如果assign失败，那么为设定该值为边界
    ASSIGN COMPONENT col_now OF STRUCTURE it_qp[ row_now ] TO FIELD-SYMBOL(<pawn_cur>).
    value = COND #( WHEN <pawn_cur> IS ASSIGNED THEN <pawn_cur> ELSE border_ins ).

  ENDMETHOD.
  "初始化棋盘 为带边界的17*17内表，其中内部15*15位空。详情可调试并观察结果
  METHOD: ini_gobang.
    "双层循环得到2-16行
    it_qp = VALUE #( FOR j = 1 UNTIL j > 15 (
                       c100 = border_ins
                       c116 = border_ins
                       "该循环得到Ctyp列，该列为表结构，存储设置为link事件的所有列
                       ctyp = VALUE #( FOR i = 1 UNTIL i > 15 (
                          columnname = |C{ i + 100 }|
                          value = if_salv_c_cell_type=>hotspot ) ) ) ).
    "循环得到一个边界工作区
    DO 17 TIMES.
      ASSIGN COMPONENT sy-index  OF STRUCTURE wa_qp TO FIELD-SYMBOL(<value>).
      <value> = border_ins.
    ENDDO.
    "将上下边界放入内表，初始化完毕
    INSERT wa_qp INTO it_qp INDEX 1.
    APPEND wa_qp TO it_qp.

  ENDMETHOD.
  "弹出消息，通常为获胜或者平局时，初始化棋盘重新开始
  METHOD: mes_gobang.
    MESSAGE mes TYPE 'I'.
    ini_gobang( ).
    dis_gobang( ).
  ENDMETHOD.
  "主显示方法
  METHOD: dis_gobang.
    "如果SALV对象不存在（第一次绘制）
    IF mo_qp IS NOT BOUND.
      ini_gobang( )."初始化棋盘
      "通过内表获取SALV对象
      cl_salv_table=>factory( IMPORTING r_salv_table = mo_qp CHANGING t_table = it_qp ).

*      mo_qp->get_functions( )->set_all( )."act all functions
*      mo_qp->get_functions( )->add_function( "附加按钮只能用于可控模式，所以会dump
*      name = 'NEWGAME' "icon = l_icon
*      text = 'New Game'
*      tooltip = 'New Game'
*      position = if_salv_c_function_position=>right_of_salv_functions ).
      "分配set_bang方法绑定到alv对象的事件 事件获取：mo_qp->get_event( )
      SET HANDLER me->set_gobang FOR mo_qp->get_event( ).
      "获取所有列对象
      DATA(gr_columns) = mo_qp->get_columns( ).
      "设定CTYP列为格式列（该列存储link事件，所以值被设定为Hotspot，详情见初始化）
      gr_columns->set_cell_type_column( 'CTYP' ).
      "循环获取所有的列，设置输出长度为2，并且居中显示，以便看上去像个棋盘
      DO 17 TIMES.
        DATA(gr_column) = gr_columns->get_column( CONV lvc_fname( |C{ sy-index + 99 }| ) )."通过列合集获取单列对象，输入参数为列明名称
        gr_column->set_output_length( 2 )."输出长度
        gr_column->set_alignment( 3 )."居中显示
      ENDDO.
      "设定表头以及其他信息，因刷新问题取消（需要重新设定）
      DATA(lo_header) = NEW cl_salv_form_layout_grid( ).
      DATA: lo_h_label TYPE REF TO cl_salv_form_label,
            lo_h_flow  TYPE REF TO cl_salv_form_layout_flow.
      lo_h_label = lo_header->create_label( row = 1 column = 1 ).
      lo_h_label->set_text( 'GoBang!' ).
      "绘制表头
      mo_qp->set_top_of_list( lo_header ).
      "绘制ALV
      mo_qp->display( ).
    ELSE.
      "如果不是第一次显示（刷新） 重新设定标题栏以及TYP列（不知为何refresh方法会把这些信息弄丢，官方文档解释refresh为rebuild）
      lo_header = NEW cl_salv_form_layout_grid( ).
      lo_h_label = lo_header->create_label( row = 1 column = 1 ).
      lo_h_label->set_text( 'GoBang!' ).
      mo_qp->get_columns( )->set_cell_type_column( 'CTYP' ).
      mo_qp->set_top_of_list( lo_header ).
      "基于行列的稳定刷新
      mo_qp->refresh( s_stable = VALUE lvc_s_stbl( row = 'X' col = 'X') refresh_mode = 2 ).
    ENDIF.

  ENDMETHOD.
  "set方法， 落子
  METHOD: set_gobang.
    "分配落子位置的值给指针，如果分配成功并且该处为空，即可落子
    ASSIGN COMPONENT column OF STRUCTURE it_qp[ row ] TO FIELD-SYMBOL(<pawn>).
    CHECK <pawn> IS INITIAL AND <pawn> IS ASSIGNED.
    "通过setp的奇偶性判断落子的值
    <pawn> = COND #( WHEN step MOD 2 EQ 1 THEN player_wht ELSE player_bak ).
    "停顿一秒 避免不必要的麻烦
    WAIT UP TO 1 SECONDS.
    step = step + 1."步数累加
    dis_gobang( ).  "刷新棋盘
    "每走一步，传入当前落子点进行胜负判断 分别基于1236四条线，方向设定见get_gobang注释。
    IF win_gobang( row = row column = CONV #( column ) off = 1 ) >= 5
    OR win_gobang( row = row column = CONV #( column ) off = 2 ) >= 5
    OR win_gobang( row = row column = CONV #( column ) off = 5 ) >= 5
    OR win_gobang( row = row column = CONV #( column ) off = 6 ) >= 5.
      "如果某条线的连续棋子总数大于5，弹出获胜消息并初始化棋盘
      mes_gobang( COND #( WHEN step MOD 2 EQ 1 THEN |You Win!| ELSE |You Lost!| ) ).
      EXIT.
    ENDIF.
    "如果步数step = 15*15，说明双方落子已经满了，并且没有分胜负，平局提示并退出
    IF step = 15 * 15.
      mes_gobang( |No Win!|  ).
      EXIT.
    ENDIF.
    "如果setp为奇数，那么刚刚落子的是玩家，此时调用AI算法落子
    CHECK step MOD 2 EQ 1.
    ai1_gobang( ).
  ENDMETHOD.
  "核心算法之AI
  "算法思路：循环棋盘上未落子的所有点进行估值，并且返回得分最高的点，调用set方法落子
  "算法分进攻和防守，可以通过微调分数设定AI智力以及走棋方式
  "可以加随机数算法使AI看起来多样化一些
  METHOD: ai1_gobang.
    "best x y为最佳落子位置
    DATA:best_x TYPE i,
         best_y TYPE i.
    "max为最高分数 因为不需要排序，只取最高，所以冒泡即可
    DATA:max TYPE i VALUE 0.

    LOOP AT it_qp INTO wa_qp.
      DATA(x1) = sy-tabix."临时变量存储内表序列（调用其他方法会改变该值）
      "只循环中间的15*15即可
      DO 16 TIMES.
        DATA(y1) = sy-index."临时变量存储do序列（不知道会不会变，保险起见用了临时变量）
        IF get_gobang( row = x1 col = y1 ) IS NOT INITIAL."如果该点已被落子，跳出循环继续
          CONTINUE.
        ENDIF.
        "进攻：对白色棋子进行估值，如果分数大约此前的分数，那么替换掉该分数，并存储当前点为best
        IF max < env_gobang( row = x1 col = y1 player = player_wht ).
          max = env_gobang( row = x1 col = y1 player = player_wht ).
          best_x = x1.
          best_y = y1.
        ENDIF.
        "防守：对黑色棋子进行估值，如果分数大约此前的分数，那么替换掉该分数，并存储当前点为best
        IF max <= env_gobang( row = x1 col = y1 player = player_bak ).
          max = env_gobang( row = x1 col = y1 player = player_bak ).
          best_x = x1.
          best_y = y1.
        ENDIF.

      ENDDO.
    ENDLOOP.
    "获取到best落子点后，调用set函数落子，
    set_gobang( row = CONV #( best_x ) column = CONV #( |C{ best_y + 99 }| ) ).

  ENDMETHOD.
  "AI核心之估值算法
  "估值算法一般来说取决于条件的齐备，但是即使条件都没问题，AI的智商看起来也不怎么高，
  "这是因为深度不够，如果估值的深度延伸到周围偏移3，就会比较厉害，但是执行慢，并且很麻烦写，
  "以下对估值算法做了一些适度优化，虽然看起来没什么用
  "传入需要估值点的坐标，需要估值的棋子类型
  METHOD: env_gobang."X Y PLAYER
    "定义对手
    DATA(opsite) = COND #( WHEN player = player_wht THEN player_bak ELSE player_wht ).
    "中心点的估值可以适当增加，这是对估值算法的优化1
    value = 16 - abs( row - 8 ) - abs( col - 8 ).
    "对8个方向的棋子循环，通过某方向的棋子数量进行相关估值
    "棋盘类型大致为活四，死四，活三，死三，活二，死二
    DO 8 TIMES.
      DATA(i) = sy-index.
*      // *11110 ("活四" 必胜 设定为最高分数)
      IF ( get_gobang( row = row col = col off = i set = 1 ) = player
       AND get_gobang( row = row col = col off = i set = 2 ) = player
       AND get_gobang( row = row col = col off = i set = 3 ) = player
       AND get_gobang( row = row col = col off = i set = 4 ) = player
       AND get_gobang( row = row col = col off = i set = 5 ) IS INITIAL ).
        value = value + 4500000.
        "优化2，测试BUG，当双方都有活四时，AI进攻会赢，却选择防守，所以输掉，所以轮到AI时要增加该点估值
        IF player EQ player_wht.value = value + 100000.ENDIF.
      ENDIF.
*         死四A 21111*
      IF ( get_gobang( row = row col = col off = i set = 1 ) = player
       AND get_gobang( row = row col = col off = i set = 2 ) = player
       AND get_gobang( row = row col = col off = i set = 3 ) = player
       AND get_gobang( row = row col = col off = i set = 4 ) = player
       AND
         ( get_gobang( row = row col = col off = i set = 5 ) = opsite
        OR get_gobang( row = row col = col off = i set = 5 ) = border_ins ) ).
        value = value + 300000.
      ENDIF.
*          死四B 111*1
      IF ( get_gobang( row = row col = col off = i set = -1 ) = player
       AND get_gobang( row = row col = col off = i set =  1 ) = player
       AND get_gobang( row = row col = col off = i set =  2 ) = player
       AND get_gobang( row = row col = col off = i set =  3 ) = player
       AND
         ( get_gobang( row = row col = col off = i set =  4 ) = opsite
        OR get_gobang( row = row col = col off = i set =  4 ) = border_ins ) ).
        value = value + 300000.
      ENDIF.
*          死四C 11*11
      IF ( get_gobang( row = row col = col off = i set = -1 ) = player
       AND get_gobang( row = row col = col off = i set = -2 ) = player
       AND get_gobang( row = row col = col off = i set =  1 ) = player
       AND get_gobang( row = row col = col off = i set =  2 ) = player
       AND
         ( get_gobang( row = row col = col off = i set =  3 ) = opsite
        OR get_gobang( row = row col = col off = i set =  3 ) = border_ins ) ).
        value = value + 300000.
      ENDIF.
*         //  2111* （活三）
      IF ( get_gobang( row = row col = col off = i set =  1 ) = player
       AND get_gobang( row = row col = col off = i set =  2 ) = player
       AND get_gobang( row = row col = col off = i set =  3 ) = player
       AND get_gobang( row = row col = col off = i set =  4 ) IS INITIAL ).
        value = value + 200000.
      ENDIF.
*         //  211* （活2）
      IF ( get_gobang( row = row col = col off = i set =  1 ) = player
       AND get_gobang( row = row col = col off = i set =  2 ) = player
       AND get_gobang( row = row col = col off = i set =  3 ) IS INITIAL ).
        value = value + 100000.
      ENDIF.
*         // 死三 11*1 2111*
      IF ( get_gobang( row = row col = col off = i set = -1 ) = player
       AND get_gobang( row = row col = col off = i set = -2 ) = player
       AND get_gobang( row = row col = col off = i set =  1 ) = opsite )
       OR
         ( get_gobang( row = row col = col off = i set =  1 ) = player
       AND get_gobang( row = row col = col off = i set =  2 ) = player
       AND get_gobang( row = row col = col off = i set = -1 ) = opsite ).
        value = value + 80000.
      ENDIF.
*// 判断是否存在 1*001（死二）
      IF ( get_gobang( row = row col = col off = i set =  1 ) = player
       AND get_gobang( row = row col = col off = i set =  2 ) IS INITIAL
       AND get_gobang( row = row col = col off = i set =  3 ) IS INITIAL ).
        value = value + 100.
      ENDIF.
*     优化3 附近点比较多的时候适当增加权重
      IF  ( get_gobang( row = row col = col off = i set = -1 ) IS NOT INITIAL
       AND  get_gobang( row = row col = col off = i set = -1 ) <> border_ins )
       OR ( get_gobang( row = row col = col off = i set =  1 ) IS NOT INITIAL
        AND get_gobang( row = row col = col off = i set =  1 ) <> border_ins ).
        value = value + 25.
      ENDIF.

    ENDDO.
    "增加随机数，适当改变攻防分数，获取不同玩法(微调)
    "可能会变得更智障
    DATA ran TYPE i.
    CALL FUNCTION 'QF05_RANDOM_INTEGER'
      EXPORTING
        ran_int_max = 2
        ran_int_min = 1
      IMPORTING
        ran_int     = ran.
    value = COND #( WHEN ran EQ 1 THEN value * 99 / 100 ELSE value * 101 / 100 ).
  ENDMETHOD.
ENDCLASS.
