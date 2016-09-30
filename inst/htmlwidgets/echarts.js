//function _getTheme(theme){
//    var myTheme;
//    require(["echarts", "ethemes" + theme], function(theme){ myTheme = theme;});
//    return myTheme;
//}

HTMLWidgets.widget({
  name: 'echarts',
  type: 'output',

  initialize: function(el, width, height) {
    return echarts.init(el);
  },


  renderValue: function(el, x, instance) {
    if (typeof(x.theme) != 'undefined') {
		if (['dark','infographic','macarons','roma','shine','vintage','red','green','blue','gray','mint','sakura','helianthus','macarons2'].indexOf(x.theme) !== -1) {
			instance = echarts.init(el, x.theme);
		}
	}

	instance.setOption(x, true);
  },

  resize: function(el, width, height, instance) {
    location.reload();
  }


});
