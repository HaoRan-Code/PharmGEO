// app-scripts.js - 应用核心JavaScript功能

$(document).ready(function() {
  // 功能卡片点击处理 
  $('.feature-card[data-tab]').on('click', function() {
    var tab = $(this).data('tab');
    Shiny.setInputValue('navigate_to', tab, {priority: 'event'});
  });
  
  // CSV下载处理器
  Shiny.addCustomMessageHandler('downloadReady', function(message) {
    var blob = new Blob([message.data], {type: 'text/csv;charset=utf-8;'});
    var link = document.createElement('a');
    if (link.download !== undefined) {
      var url = URL.createObjectURL(blob);
      link.setAttribute('href', url);
      link.setAttribute('download', message.filename);
      link.style.visibility = 'hidden';
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
    }
  });
  
  // Excel下载处理器
  Shiny.addCustomMessageHandler('downloadExcel', function(message) {
    var byteCharacters = atob(message.content);
    var byteNumbers = new Array(byteCharacters.length);
    for (var i = 0; i < byteCharacters.length; i++) {
      byteNumbers[i] = byteCharacters.charCodeAt(i);
    }
    var byteArray = new Uint8Array(byteNumbers);
    var blob = new Blob([byteArray], {type: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'});
    
    var link = document.createElement('a');
    if (link.download !== undefined) {
      var url = URL.createObjectURL(blob);
      link.setAttribute('href', url);
      link.setAttribute('download', message.filename);
      link.style.visibility = 'hidden';
      document.body.appendChild(link);
      link.click();
      document.body.removeChild(link);
    }
  });
});

// ECharts处理器 - 优化版本
var echartResizeHandler; 
Shiny.addCustomMessageHandler('echarts_data', function(data) {
  var chartDom = document.getElementById('echart_container');
  if (!chartDom) return;
  
  var myChart = echarts.getInstanceByDom(chartDom) || echarts.init(chartDom);
  chartDom.myChartInstance = myChart;
  
  if (!data || !data.nodes || data.nodes.length === 0) {
    myChart.setOption({ 
      title: { text: 'No interaction data', left: 'center', textStyle: { color: '#757575'} } 
    }, true);
    return;
  }
  
  // 优化的分类颜色配置
  var categoryColors = {
    'A_up_B_down':'#ff5252', 'A_down_B_up':'#ff9100',
    'A_up_B_up':'#00b686', 'A_down_B_down':'#42a5f5',
    'Other':'#757575', 'DrugA':'#4158D0', 'DrugB':'#C850C0'
  };
  
  // 动态处理分类颜色
  if (data.categories) {
    data.categories.forEach(function(category) {
      var catName = category.name;
      if (catName.includes('_up_') && catName.includes('_down')) {
        categoryColors[catName] = '#ff5252';
      } else if (catName.includes('_down_') && catName.includes('_up')) {
        categoryColors[catName] = '#ff9100';
      } else if (catName.includes('_up_') && catName.endsWith('_up')) {
        categoryColors[catName] = '#00b686';
      } else if (catName.includes('_down_') && catName.endsWith('_down')) {
        categoryColors[catName] = '#42a5f5';
      }
    });
  }
  
  // 节点位置和样式处理
  var processedNodes = data.nodes.map(function(node, index) {
    var cat = node.category;
    var x, y;
    var isDrugA = cat === 'DrugA' || (data.drug_a_category && cat === data.drug_a_category);
    var isDrugB = cat === 'DrugB' || (data.drug_b_category && cat === data.drug_b_category);
    var isDrug = isDrugA || isDrugB;
    
    if (isDrugA) { x = 100; y = 300; }
    else if (isDrugB) { x = 700; y = 300; }
    else {
      var angle = (index / data.nodes.length) * 2 * Math.PI;
      x = 400 + 150 * Math.cos(angle);
      y = 300 + 150 * Math.sin(angle);
    }
    
    return {
      id: node.id, name: node.name,
      category: node.display_category || node.category,
      original_category: node.category,
      x: x, y: y,
      symbolSize: isDrug ? 30 : 20,
      itemStyle: {
        color: categoryColors[node.category] || '#4158D0',
        borderColor: '#fff', borderWidth: 2,
        shadowBlur: 10, shadowColor: 'rgba(0,0,0,0.1)'
      }
    };
  });
  
  // 处理分类和连接
  var processedCategories = data.categories ? data.categories.map(function(c) {
    return {
      name: c.display_name || c.name,
      itemStyle: { color: categoryColors[c.name] || '#4158D0' }
    };
  }) : [];
  
  var processedLinks = data.links ? data.links.map(function(e) {
    return {
      source: e.source, target: e.target,
      symbol: ['none', 'arrow'], symbolSize: [0, 8],
      lineStyle: { color: 'source', opacity: 0.7, width: 1.5, curveness: 0.2 }
    };
  }) : [];
  
  // ECharts配置
  var graphOption = {
    backgroundColor: '#fff',
    tooltip: {
      trigger: 'item',
      formatter: function(p) {
        if (p.dataType === 'node') {
          return '<div style="padding:8px"><span style="font-weight:bold">' + p.data.name + 
                 '</span><br><span>Category: ' + p.data.category + '</span></div>';
        }
        return p.name;
      },
      backgroundColor: 'rgba(255,255,255,0.95)',
      borderColor: '#e6e6e6', borderWidth: 1
    },
    legend: {
      data: processedCategories.map(function(a) { return a.name; }),
      top: 'bottom'
    },
    series: [{
      type: 'graph', layout: 'none', animation: false,
      roam: true, draggable: true,
      label: {
        show: true, position: 'right',
        formatter: function(p) {
          var isDrug = p.data.original_category === 'DrugA' || p.data.original_category === 'DrugB' ||
                      (data.drug_a_category && p.data.original_category === data.drug_a_category) ||
                      (data.drug_b_category && p.data.original_category === data.drug_b_category);
          if (isDrug) return p.data.name;
          var g = p.data.name;
          return g.length > 6 ? g.substring(0, 6) + '...' : g;
        },
        fontSize: 12
      },
      data: processedNodes,
      categories: processedCategories,
      edges: processedLinks
    }]
  };
  
  myChart.setOption(graphOption, true);
});

// ECharts调整大小处理器
Shiny.addCustomMessageHandler('resize_ddi_echart', function(message) {
  var chartDom = document.getElementById('echart_container');
  if (chartDom && chartDom.myChartInstance) {
    setTimeout(function(){ chartDom.myChartInstance.resize(); }, 200);
  }
}); 