(function (root, factory) {
    if (typeof define === 'function' && define.amd) {
        // AMD. Register as an anonymous module.
        define(['exports', 'echarts'], factory);
    } else if (typeof exports === 'object' && typeof exports.nodeName !== 'string') {
        // CommonJS
        factory(exports, require('echarts'));
    } else {
        // Browser globals
        factory({}, root.echarts);
    }
}(this, function (exports, echarts) {
    var log = function (msg) {
        if (typeof console !== 'undefined') {
            console && console.error && console.error(msg);
        }
    };
    if (!echarts) {
        log('ECharts is not Loaded');
        return;
    }

    var colorPalette = ['#ed9678','#e7dac9','#cb8e85','#f3f39d','#c8e49c',
            '#f16d7a','#f3d999','#d3758f','#dcc392','#2e4783',
            '#82b6e9','#ff6347','#a092f1','#0a915d','#eaf889',
            '#6699FF','#ff6666','#3cb371','#d5b158','#38b6b6'
	];


    var theme = {
        color: colorPalette,

        title: {
            textStyle: {
                fontWeight: 'normal',
                color: '#008acd'
            }
        },

        visualMap: {
            itemWidth: 15,
            color: ['#cb8e85','#e7dac9'],
			textStyle: {
				color: '#333'          // 值域文字颜色
			}
        },
		
        toolbox: {
            iconStyle: {
                normal: {
                    borderColor: colorPalette[0]
                }
            }
        },

        tooltip: {
            backgroundColor: 'rgba(50,50,50,0.5)',
            axisPointer : {
                type : 'line',
                lineStyle : {
                    color: '#008acd'
                },
                crossStyle: {
                    color: '#008acd'
                },
                shadowStyle : {
                    color: 'rgba(200,200,200,0.2)'
                }
            }
        },

        dataZoom: {
            dataBackgroundColor: '#efefff',
            fillerColor: 'rgba(182,162,222,0.2)',
            handleColor: '#008acd'
        },

        grid: {
            borderColor: '#eee'
        },

        categoryAxis: {
            axisLine: {
                lineStyle: {
                    color: '#008acd'
                }
            },
            splitLine: {
                lineStyle: {
                    color: ['#eee']
                }
            }
        },

        valueAxis: {
            axisLine: {
                lineStyle: {
                    color: '#008acd'
                }
            },
            splitArea : {
                show : true,
                areaStyle : {
                    color: ['rgba(250,250,250,0.1)','rgba(200,200,200,0.1)']
                }
            },
            splitLine: {
                lineStyle: {
                    color: ['#eee']
                }
            }
        },

        timeline : {
            lineStyle : {
                color : '#008acd'
            },
            controlStyle : {
                normal : { color : '#008acd'},
                emphasis : { color : '#008acd'}
            },
            symbol : 'emptyCircle',
            symbolSize : 3
        },

		// 柱形图默认参数
		bar: {
			barMinHeight: 0,          // 最小高度改为0
			// barWidth: null,        // 默认自适应
			barGap: '30%',            // 柱间距离，默认为柱形宽度的30%，可设固定值
			barCategoryGap : '20%',   // 类目间柱形距离，默认为类目间距的20%，可设固定值
			itemStyle: {
				normal: {
					// color: '各异',
					barBorderColor: '#fff',       // 柱条边线
					barBorderRadius: 0,           // 柱条边线圆角，单位px，默认为0
					barBorderWidth: 1            // 柱条边线线宽，单位px，默认为1					
				},
				emphasis: {
					// color: '各异',
					barBorderColor: 'rgba(0,0,0,0)',   // 柱条边线
					barBorderRadius: 0,                // 柱条边线圆角，单位px，默认为0
					barBorderWidth: 1                 // 柱条边线线宽，单位px，默认为1					
				}
			},
			label: {
				normal: {
					show: false
				},
				emphasis: {
					show: false
				}
			}
		},
    
		// 饼图默认参数
		pie: {
			center : ['50%', '50%'],    // 默认全局居中
			radius : [0, '75%'],
			clockWise : false,          // 默认逆时针
			startAngle: 90,
			minAngle: 0,                // 最小角度改为0
			selectedOffset: 10,         // 选中是扇区偏移量
			itemStyle: {
				normal: {
					// color: 各异,
					borderColor: '#fff',
					borderWidth: 1
				}
			},
			label: {
				normal: {
					show: true,
					position: 'outer',
					textStyle: {color: '#1b1b1b'},
					lineStyle: {color: '#1b1b1b'}
						// textStyle: null      // 默认使用全局文本样式，详见TEXTSTYLE
				}
			},
			labelLine: {
				normal: {
					show: true,
					length: 20,
					lineStyle: {
						// color: 各异,
						width: 1,
						type: 'solid'
					}
				}
			}
		}, 

		// 折线图默认参数
		line: {
			itemStyle: {
				normal: {
					// color: 各异,
					label: {
						show: false
						// position: 默认自适应，水平布局为'top'，垂直布局为'right'，可选为
						//           'inside'|'left'|'right'|'top'|'bottom'
						// textStyle: null      // 默认使用全局文本样式，详见TEXTSTYLE
					},
					lineStyle: {
						width: 2,
						type: 'solid',
						shadowColor : 'rgba(0,0,0,0)', //默认透明
						shadowBlur: 5,
						shadowOffsetX: 3,
						shadowOffsetY: 3
					}
				},
				emphasis: {
					// color: 各异,
					label: {
						show: false
						// position: 默认自适应，水平布局为'top'，垂直布局为'right'，可选为
						//           'inside'|'left'|'right'|'top'|'bottom'
						// textStyle: null      // 默认使用全局文本样式，详见TEXTSTYLE
					}
				}
			},
			//smooth : false,
			//symbol: null,         // 拐点图形类型
			symbolSize: 3,          // 拐点图形大小
			//symbolRotate : null,  // 拐点图形旋转控制
			showAllSymbol: false    // 标志图形默认只有主轴显示（随主轴标签间隔隐藏策略）
		},
		
		// K线图默认参数
		candlestick: {
			// barWidth : null          // 默认自适应
			// barMaxWidth : null       // 默认自适应 
			itemStyle: {
				normal: {
					color: '#fe9778',          // 阳线填充颜色
					color0: '#e7dac9',      // 阴线填充颜色
					lineStyle: {
						width: 1,
						color: '#f78766',   // 阳线边框颜色
						color0: '#f1ccb8'   // 阴线边框颜色
					}
				},
				emphasis: {
					// color: 各异,
					// color0: 各异
				}
			}
		},
	
		map: {
			showLegendSymbol : true,       // 显示图例颜色标识（系列标识的小圆点），存在legend时生效
			label: {
                normal: {
                    textStyle: {
						color: 'rgba(139,69,19,1)'
					}
                },
				emphasis: {
					textStyle: {
						color: 'rgba(139,69,19,1)'
					}
				}
            },
			itemStyle: {
				normal: {
					// color: 各异,
					borderColor: '#fff',
					borderWidth: 1,
					areaColor: '#ccc'//rgba(135,206,250,0.8)
				},
				emphasis: {                 // 也是选中样式
					// color: 各异,
					borderColor: 'rgba(0,0,0,0)',
					borderWidth: 1,
					areaColor: '#f3f39d'
				}
			}
		},
		
		graph : {
			color: colorPalette,
			itemStyle: {
				normal: {
					// color: 各异,					
					nodeStyle : {
						brushType : 'both',
						strokeColor : '#a17e6e'
					},
					linkStyle : {
						strokeColor : '#a17e6e'
					}
				},
				emphasis: {
					// color: 各异,					
					nodeStyle : {},
					linkStyle : {}
				}
			},
			label: {
				normal:{
					show: false
				},
				emphasis: {
					show: false
				}
			}
		},
		
		gauge : {
			axisLine: {            // 坐标轴线
				show: true,        // 默认显示，属性show控制显示与否
				lineStyle: {       // 属性lineStyle控制线条样式
					color: [[0.2, '#ed9678'],[0.8, '#e7dac9'],[1, '#cb8e85']], 
					width: 8
				}
			},
			axisTick: {            // 坐标轴小标记
				splitNumber: 10,   // 每份split细分多少段
				length :12,        // 属性length控制线长
				lineStyle: {       // 属性lineStyle控制线条样式
					color: 'auto'
				}
			},
			axisLabel: {           // 坐标轴文本标签，详见axis.axisLabel
				textStyle: {       // 其余属性默认使用全局文本样式，详见TEXTSTYLE
					color: 'auto'
				}
			},
			splitLine: {           // 分隔线
				length : 18,         // 属性length控制线长
				lineStyle: {       // 属性lineStyle（详见lineStyle）控制线条样式
					color: 'auto'
				}
			},
			pointer : {
				length : '90%',
				color : 'auto',
				width: 5
			},
			title : {
				textStyle: {       // 其余属性默认使用全局文本样式，详见TEXTSTYLE
					color: '#333'
				}
			},
			detail : {
				textStyle: {       // 其余属性默认使用全局文本样式，详见TEXTSTYLE
					color: 'auto'
				}
			}
		}
    };

    echarts.registerTheme('macarons2', theme);
}));
