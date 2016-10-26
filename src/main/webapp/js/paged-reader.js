(function($){
    /**
     * Скрипт устанавливает понятие content page и
     * rendered page. Изначально книга
     * каким то образом разбита на куски ("content pages"), возможно на главы или на
     * равные промежутки текста. Rest сервисы предоставляют
     * доступ к кускам и к описательным файлам. Скрипт подгружает куски и при помощи
     * скрипта columnizer, бьет их на страницы для отображения - "rendered pages".
     * http://sharedfil.es/js-48hIfQE4XK.html
     */

    pagedReader = function(options) {
        var defaults = {
            prevButtonSelector: '#page-prev-button',
            nextButtonSelector: '#page-next-button',
            gotoHomeSelector: '#goto-home',
            pageHeaderSelector: '#page-header',
            pageContentSelector: '#page-content',
            rightColumnSelector: '#right-column',
            leftColumnSelector: '#left-column',
            pageBottomSelector: '#page-bottom',
            bookRegionSelector: '#book-region',
            pageRegionSelector: '#page-region',
            pageNumberSelector: '#page-number',
            showContentsSelector: '#show-contents',
            translateSelector: "#translate",
            pageTitleSelector: '#page-title',
            pageSliderSelector: '#slider',
            pieSpanSelector: '#pie-span',
            pageBackwardSelector: '#page-backward',
            contentsModalSelector: '#contents-modal',
            contentsSelector: '#contents-modal-body',
            tipDialogSelector: '#tip-dialog',
            tipDialogContentSelector: '#tip-dialog-content',
            showOptionsSelector: '#show-options',
            closeOptionsSelector: '#close-option',
            optionsDialogSelector: '#options-dialog',
            optionFontSizeSelector: '#option-font-size',
            optionFontFamilySelector: '#option-font-family',
            optionTextAlignLeftSelector: '#option-align-left',
            optionTextAlignJustifySelector: '#option-align-justify',
            optionWhiteThemeSelector: '#option-theme-white',
            optionSepiaThemeSelector: '#option-theme-sepia',
            optionOldschoolThemeSelector: '#option-theme-oldschool',
            topPanelSelector: '#top-panel',
            bottomPanelSelector: '#bottom-panel',
            bookId: '0',
            fontFamily: 'arial',
            fontSize: '10pt',
            textAlign: 'left',
            theme: 'white',
            lastPosition: 0.0,
            link: ''
        };

        options = $.extend(defaults, options);

        $.ajaxSetup({async: false});

        var contentHeight = 500;

        var pies = [];
        var pages = [];
        var tips = [];

        var currentPie = null;
        var currentPageNumber = 0;

        var doublePage = false;
        var step = 1;
        var minPageWidth = 300;
        var linksChain = [];
        var inputTimeoutDone = true;
        var contents = null;

        function gotoContents(id) {
            $(options.contentsModalSelector).modal('hide');
            linksChain = [];
            gotoLink(id);
        }

        function gotoBackward() {
            var id = linksChain[linksChain.length - 1].backwardId;
            linksChain.pop();
            if(linksChain.length < 1) {
                $(options.pageBackwardSelector).hide();
            }
            gotoLink(id);
        }

        function gotoLinkWithBackward(id, backId) {
            var tipIndex = -1;
            $.each(tips, function(index, tipIterator){
                if(tipIterator.tip == id){
                    tipIndex = index;
                }
            });

            if(tipIndex < 0) {
                linksChain.push({
                    backwardId: backId,
                    currentId: id
                });
                gotoLink(id);
                $(options.pageBackwardSelector).show();
            } else {
                modalTip(tips[tipIndex].content)
            }
        }

        function hidePanels() {
            $(options.bottomPanelSelector).fadeOut( "slow", function() {});
            $(options.pageTitleSelector).fadeOut( "slow", function() {});
            $(options.pageNumberSelector).fadeOut( "slow", function() {});
            $(options.gotoHomeSelector).fadeOut( "slow", function() {});
            $(options.showOptionsSelector).fadeOut( "slow", function() {});
            $(options.showContentsSelector).fadeOut( "slow", function() {});
        }

        function fastHidePanels() {
            $(options.bottomPanelSelector).hide()
            $(options.pageTitleSelector).hide()
            $(options.pageNumberSelector).hide()
            $(options.gotoHomeSelector).hide()
            $(options.showOptionsSelector).hide()
            $(options.showContentsSelector).hide()
        }

        function showPanels() {
            $(options.pageTitleSelector).fadeIn( "slow", function() {});
            $(options.pageNumberSelector).fadeIn( "slow", function() {});
            $(options.gotoHomeSelector).fadeIn( "slow", function() {});
            $(options.showOptionsSelector).fadeIn( "slow", function() {});
            $(options.showContentsSelector).fadeIn( "slow", function() {});
            $(options.bottomPanelSelector).fadeIn( "slow", function() {});
        }

        function turnPanelsVisibility() {
            if($(options.bottomPanelSelector).css('display') == 'none')
                showPanels()
            else
                hidePanels()
        }

        function optionsDialog() {
            $( options.optionsDialogSelector ).dialog({
                draggable: false,
                show: true,
                hide: true,
                maxWidth: 350,
                autoOpen: false,
                modal: true,
                title: null,
                open: function(){
                    jQuery('.ui-widget-overlay').bind('click',function(){
                        jQuery(options.optionsDialogSelector).dialog('close');
                    });
                    jQuery(options.closeOptionsSelector).bind('click',function(){
                        jQuery(options.optionsDialogSelector).dialog('close');
                    });
                    jQuery('.ui-widget-content').bind('click',function(){
                    });
                }
            });
            $(options.optionsDialogSelector).dialog("open");
            $(".ui-dialog-titlebar").hide();
        }

        function modalTip(content) {
            $( options.tipDialogContentSelector ).empty();
            $( options.tipDialogContentSelector ).append(content);

            $( options.tipDialogSelector ).dialog({
                minHeight: 'auto',
                draggable: false,
                show: true,
                hide: true,
                bgiframe: true,
                resizable: false,
                resize: 'auto',
                //width: $(options.pageContentSelector).width(),
                width: Math.max($(options.pageContentSelector).width() / 2, 320),
                autoOpen: false,
                modal: true,
                title: null,
                open: function(){
                    jQuery('.ui-widget-overlay').bind('click',function(){
                        jQuery(options.tipDialogSelector).dialog('close');
                    });
                    jQuery('.ui-widget-content').bind('click',function(){
                        jQuery(options.tipDialogSelector).dialog('close');
                    });
                }
            });

            $(options.tipDialogSelector).dialog("open");
            $(".ui-dialog-titlebar").hide();
        }

        function loadOptionsDialog() {
            setTextAlign(options.textAlign);
            setFontFamily(options.fontFamily);
            setFontSize(options.fontSize);
            setTheme(options.theme);

            $(options.optionTextAlignLeftSelector).bind('click',function(){
                setTextAlign('left');
                resizeFunction();
            });

            $(options.optionTextAlignJustifySelector).bind('click',function(){
                setTextAlign('justify');
                resizeFunction();
            });

            $(options.optionFontFamilySelector).change(function() {
                setFontFamily($(this).val());
                resizeFunction();
            });

            $(options.optionFontSizeSelector).change(function() {
                setFontSize($(this).val());
                resizeFunction();
            });

            $(options.optionWhiteThemeSelector).bind('click',function(){
                setTheme('white');
            });

            $(options.optionSepiaThemeSelector).bind('click',function(){
                setTheme('sepia');
            });

            $(options.optionOldschoolThemeSelector).bind('click',function(){
                setTheme('oldschool');
            });
        }

        function saveOptions() {
            $.ajax({async: true,
                type: 'POST',
                data: {
                    'font-size': options.fontSize,
                    'font-family': options.fontFamily,
                    'text-align': options.textAlign,
                    'theme': options.theme
                },
                url: '/set-my-options'});
        }

        function setTheme(inTheme) {
            theme = inTheme;
            if(theme == '') theme = 'white';
            options.theme = theme;

            if(theme == 'white') {
                $('body').css({'background-color': '#fff'});
                $('body').css({'color': '#333333'});
            }

            if(theme == 'sepia') {
                $('body').css({'background-color': 'rgba(142, 133, 7, 0.05)'});
                $('body').css({'color': '#333333'});
            }

            if(theme == 'oldschool') {
                $('body').css({'background-color': '#0000ff'});
                $('body').css({'color': '#fff'});
            }

            saveOptions();
        }

        function setFontSize(fontSize) {
            $(options.optionFontSizeSelector).val(fontSize);
            options.fontSize = fontSize;
            $(options.bookRegionSelector).css({
                'font-size': fontSize
            });
            $(options.tipDialogContentSelector).css({
                'font-size': fontSize
            });

            saveOptions();
        }

        function setFontFamily(family) {
            $(options.optionFontFamilySelector).val(family);
            options.fontFamily = family;
            $(options.bookRegionSelector).css({
                fontFamily: family
            });
            $(options.tipDialogContentSelector).css({
                fontFamily: family
            });
            saveOptions();
        }

        function setTextAlign(align) {
            if(align.toString() == 'left'){
                $(options.bookRegionSelector).css({
                    textAlign: 'left'
                });
                options.textAlign = 'left';
                $(options.optionTextAlignLeftSelector).addClass('active');
                $(options.optionTextAlignJustifySelector).removeClass('active');
            } else {
                $(options.bookRegionSelector).css({
                    textAlign: 'justify'
                });
                options.textAlign = 'justify';
                $(options.optionTextAlignJustifySelector).addClass('active');
                $(options.optionTextAlignLeftSelector).removeClass('active');
            }
            saveOptions();
        }

        function selectElement(id) {
            $('#' + id).css({backgroundColor: '#88AA77'});
        }

        /**
         * Функция вызывается при загрузке структуры документа.
         * Получает описание книги, устанавливает текущую позицию,
         * устанавливает интерактивные свойства элекментов:
         * отработка перелистывания.
         */
        var resizeTimer;
        var hidePanelsTimer;
        var textWasSelected = false;

        $(document).ready(function() {

            /*$(document).bind(
                'touchmove',
                function(e) {
                    e.preventDefault();
                }
            );*/

            function textSelected() {
                $(options.translateSelector).show();
                textWasSelected = true;
            }

            function textNotSelected() {
                if(textWasSelected != true) turnPanelsVisibility();
                $(options.translateSelector).hide();
                textWasSelected = false;
            }
            $(options.translateSelector).hide();

            function checkTextSelected() {
                var text = getSelectedText();
                if (text!='')
                    textSelected();
                else
                    textNotSelected();
            }

            if(!isMobile()) {
                $(options.pageContentSelector).mouseup(checkTextSelected);
                $(options.pageContentSelector).dblclick(checkTextSelected);
            } else {
                document.getElementById("page-content").addEventListener("touchend", checkTextSelected, false);
                $(options.pageContentSelector).mouseup(function() {
                    if(getSelectedText() == '')
                        $(options.translateSelector).hide();
                });
            }

            $(options.translateSelector).click(translateSelectedText)

            function translateSelectedText() {
                var text = getSelectedText();
                if (text!='') {
                    $.ajax({async: false,
                        type: 'POST',
                        dataType: "json",
                        data: {
                            'text': text
                        },
                        url: '/translate',
                        success: function(data) {
                            modalTip($("<span><b>" + text + "</b>" + "<br/>" + data.result + "" +
                                "</span>"));
                            $(options.translateSelector).hide();
                    }});
                }
            }

            function getSelectedText() {
                if (window.getSelection) {
                    return window.getSelection().toString();
                } else if (document.selection) {
                    return document.selection.createRange().text;
                }
                return '';
            }

            $(window ).resize(function() {
                $(options.pageContentSelector).empty();
                $(options.pageContentSelector).html('');
                clearTimeout(resizeTimer);
                resizeTimer = setTimeout(resizeFunction, 300);
            });

            $(window).on("orientationchange",function(){
                $(options.pageContentSelector).empty();
                $(options.pageContentSelector).html('');
                clearTimeout(resizeTimer);
                resizeTimer = setTimeout(resizeFunction, 300);
            });

            $.getJSON('/book/' + options.bookId + '/description.json', function(data, textStatus, jqXHR){
                pies = data.pages;
                tips = data.tips;
            });

            $.ajax({async: false,
                type: 'GET',
                url: '/book/' + options.bookId + '/contents.html',
                success: function(data) {
                    contents = data;
                }});

            $(options.gotoHomeSelector).click(function(){
                window.location = '/view?id=' + options.bookId
            });


            /*$(options.pageBottomSelector).hover(function(){
                clearTimeout(hidePanelsTimer);
                showPanels();
            }, function() {
                hidePanelsTimer = setTimeout(hidePanels, 1000);
            });

            $(options.pageHeaderSelector).hover(function(){
                clearTimeout(hidePanelsTimer);
                showPanels();
            }, function() {
                hidePanelsTimer = setTimeout(hidePanels, 1000);
            });

            $(options.bottomPanelSelector).hover(function(){
                clearTimeout(hidePanelsTimer);
                showPanels();
            }, function() {
                hidePanelsTimer = setTimeout(hidePanels, 1000);
            });*/

            $(options.showOptionsSelector).click(function(){
                optionsDialog();
            });

            function toggleGotoPrevPage() {
                if(inputTimeoutDone){
                    gotoPrevPage();
                    inputTimeoutDone = false;
                    setTimeout(function(){
                        inputTimeoutDone = true
                    }, 100);
                }
            }

            function toggleGotoNextPage() {
                if(inputTimeoutDone) {
                    var start = new Date();
                    gotoNextPage();
                    inputTimeoutDone = false;
                    setTimeout(function(){
                        inputTimeoutDone = true
                    }, 100);
                    var end = new Date();
                    //alert('Скорость ' + (end.getTime()-start.getTime()) + ' мс');
                }
            }

            $(window).keydown(function(event){
                switch (event.keyCode) {
                    case 37:
                        toggleGotoPrevPage();
                        break
                    case 39:
                        toggleGotoNextPage();
                        break
                }
            });

            $(options.leftColumnSelector).click(function(){
                toggleGotoPrevPage()
            });

            $(options.rightColumnSelector).click(function(){
                toggleGotoNextPage()
            });

            $(options.pageBackwardSelector).click(function(){
                if(inputTimeoutDone){
                    gotoBackward();
                    inputTimeoutDone = false;
                    setTimeout(function(){
                        inputTimeoutDone = true
                    }, 100);
                }
            });

            $(options.pageSliderSelector).slider({
                min: 0,
                max: 1,
                range: false,
                step: 0.00001,
                slide: function( event, ui ) {
                    $(options.pieSpanSelector).empty();
                    var realPos = $(options.pageSliderSelector).slider( "value" );
                    var roundedPos = Math.round(realPos * 1000) / 10;
                    $(options.pieSpanSelector).append(roundedPos);
                },
                stop: function( event, ui ) {
                    var realPos = $(options.pageSliderSelector).slider( "value" );
                    gotoPositionBySlider(realPos);
                }
            });

            //hidePanelsTimer = setTimeout(hidePanels, 2000);
            $(options.pageBackwardSelector).hide();
            loadOptionsDialog();
            initializeBook(true)

            if(options.link != '') {
                gotoLink(options.link);
            } else {
                gotoPosition(options.lastPosition)
            }

        });

        function pageNumberByPosition(position) {
            var result = Math.floor(pages.length *
                Math.min((position - currentPie.pieMin) / currentPie.pie, 0.999999999))
            if(doublePage && !isEven(result))
                result = result - 1
            return result;
        }

        function setPositionByCurrentPageNumber() {
            var position = currentPie.pieMin +
                ((currentPageNumber / pages.length) * currentPie.pie);
            saveLastPosition(position);
            setSliderPos(position);
        }

        function gotoPositionBySlider(position) {
            renderPie(findPie(position), false);
            currentPageNumber = pageNumberByPosition(position);
            showCurrentPage();
            saveLastPosition(position);
        }

        function gotoPosition(position) {
            renderPie(findPie(position), false);
            currentPageNumber = pageNumberByPosition(position);
            showCurrentPage();
            saveLastPosition(position);
            setSliderPos(position);
        }

        function gotoLink(id) {

            var needPie = findPieById(id);
            renderPie(needPie, false);

            var number = 0;

            $.each(pages, function(index, pageIterator){
                var sel = '#page-content-center';
                if(doublePage) sel = '#page-content-right';

                $(sel).empty();
                $(sel).html(pageIterator);
                var elem = $(sel + ' #' + id);

                if($(sel).has('#' + id).length) {
                    var elem = $(sel + ' #' + id);
                    number = index;
                    return false;
                }
            });

            currentPageNumber = number;
            setPositionByCurrentPageNumber();
            showCurrentPage();
        }

        function gotoNextPage() {
            currentPageNumber = currentPageNumber + step;
            if(currentPageNumber >= (pages.length)){
                currentPageNumber = currentPageNumber - step;
                renderNextPie();
            }
            setPositionByCurrentPageNumber();
            showCurrentPage();
        }

        function gotoPrevPage() {
            currentPageNumber = currentPageNumber - step;
            if(currentPageNumber < 0) {
                currentPageNumber = currentPageNumber + step;
                renderPrevPie();
            }
            setPositionByCurrentPageNumber();
            showCurrentPage();
        }

        function resizeFunction() {
            clearPages();
            currentPie = -1;
            initializeBook(true);
            gotoPosition(options.lastPosition);
        }

        function makePages() {
            $(options.pageContentSelector).empty();
            var fieldWidth = $(options.pageContentSelector).outerWidth(true);
            $('body').css('position', 'fixed');
            if((minPageWidth * 2) < fieldWidth) {
                doublePage = true;
                $(options.pageContentSelector).append($("<div id='page-content-left' style='width: 50%; float: left;'></div>"));
                $(options.pageContentSelector).append($("<div id='page-content-right' style='width: 50%; float: left;'></div>"));
                step = 2;
            } else {
                $(options.pageContentSelector).append($("<div id='page-content-center' style='width: 100%;'></div>"));
                doublePage = false;
                step = 1;
            }
        }

        function initializeBook(rerender) {
            makePages();

            contentHeight = document.body.clientHeight - $(options.pageHeaderSelector).outerHeight(true) -
                $(options.pageBottomSelector).outerHeight(true);

            $(options.contentsSelector).empty();
            $(options.contentsSelector).html(contents);

            $(options.prevButtonSelector).css('margin-top', (contentHeight / 2) + 'px');
            $(options.nextButtonSelector).css('margin-top', (contentHeight / 2) + 'px');

            $(options.pageContentSelector).height(contentHeight);
            $(options.rightColumnSelector).height(contentHeight);
            $(options.leftColumnSelector).height(contentHeight);
        }



        function setSliderPos(pie) {
            $(options.pageSliderSelector).slider('value', pie);
            var roundedPos = (pie * 100).toFixed(1)
            $(options.pieSpanSelector).empty();
            $(options.pieSpanSelector).append(roundedPos);
        }


        /**
         * Отобразить текущую currentRenderedPageNumber rendered page
         */
        function isEven(num) { return !(num % 2);}

        function showCurrentPage() {
            var pageNumber = String(currentPageNumber + 1);

            if(doublePage) {
                var rightNumber = currentPageNumber;
                var leftNumber = currentPageNumber - 1;

                if(isEven(currentPageNumber)){
                    var leftNumber = currentPageNumber;
                    var rightNumber = currentPageNumber + 1;
                }
                pageNumber = String(leftNumber + 1) + '-' + String(rightNumber + 1);

                $('#page-content-left').empty();
                if(leftNumber > 1)
                    $("#page-content-left").append('<div class="top-line" id="top-line-left"><div>');
                $("#page-content-left").append(pages[leftNumber]);
                $('#page-content-right').empty();
                $("#page-content-right").append('<div class="top-line" id="top-line-right"><div>');
                $("#page-content-right").append(pages[rightNumber]);
            } else {
                $('#page-content-center').empty();
                if(currentPageNumber > 1)
                    $("#page-content-center").append('<div class="top-line" id="top-line-center"><div>');
                $("#page-content-center").append(pages[currentPageNumber]);
            }

            $(options.pageNumberSelector).empty();
            var pagesLength = pages.length;
            if(!isEven(pages.length) && doublePage)
                pagesLength = pagesLength + 1;
            $(options.pageNumberSelector).html(pageNumber + "/" + String(pagesLength));

            $('a[goto]').each(function(index) {
                $(this).unbind('click');
                $(this).bind('click',function(){
                    gotoLinkWithBackward($(this).attr('goto'), $(this).attr('id'));
                    false
                });
            });

            $('a[goto-contents]').each(function(index) {
                $(this).unbind('click');
                $(this).bind('click',function(){
                    gotoContents($(this).attr('goto-contents'));
                    false
                });
            });

            if(linksChain.length > 0) {
                selectElement(linksChain[linksChain.length - 1].currentId);
            }
        }

        /**
         * Получить pie с заданным номером
         * @param number
         * @returns content page
         */
        function getPieByNumber(number) {
            var result = null;
            $.each(pies, function(index, pageIterator){
                if(pageIterator.number == number){
                    result = pageIterator;
                }
            });
            return result;
        }

        function renderNextPie() {
            if(currentPie.number < (pies.length - 1)) {
                renderPie(getPieByNumber(currentPie.number + 1));
                currentPageNumber = 0;
            }

        }

        function renderPrevPie() {
            if(currentPie.number > 0) {
                renderPie(getPieByNumber(currentPie.number - 1));
                currentPageNumber = pages.length - 1;
            }
        }

        function clearPages() {
            pages = [];
        }

        /**
         * Отрисовывает в rendered pages заданную content page,
         * @param pie заданная content page.
         * @param rerender перерисовка в любом случае.
         */
        function renderPie(pie, rerender) {
            if(rerender || pie != currentPie) {
                clearPages();
                renderPieData(getData(pie));
                currentPie = pie;
            }
        }

        function renderPieData(pieData) {
            var targetSelector = "#page-content-center";
            if(doublePage) targetSelector = "#page-content-left";
            if($(pieData).length > 0){
                $.paginate({
                    srcPrefix: '/book/' + options.bookId + '/',
                    accuracy: 20,
                    source: $(pieData),
                    target: targetSelector,
                    height: contentHeight,
                    doneFunc: function(resultPages){
                        pages = resultPages;
                        $(targetSelector).empty();
                    }
                });
            }
        }

        /**
         * Запомнить позицию книги
         * @param position
         */
        function saveLastPosition(position) {
            options.lastPosition = position
            $.ajax({async: true,
                type: 'POST',
                data: {
                    'position': position
                },
                url: '/set-position/book/' + options.bookId});
        }

        /**
         * Загрузить, если не загружено поле data для content page с помощью rest сервиса.
         * @param pie.
         * @returns загруженные данные.
         */
        function getData(pie) {
            if(pie.data == null) {
                $.ajax({async: false,
                    type: 'GET',
                    url: '/book/' + options.bookId + '/' + String(pie.number) + ".pie",
                    success: function(data) {
                        pie.data = data;
                    }});
            }
            if(pie.data == null) {
                modalTip('<b>К сожалению связь с сервером прервалась, проверьте подключение к интернету</b>');
            }
            asyncLoadPies(pie);
            return pie.data;
        }

        function asyncLoadPies(pie) {
            prevPie = getPieByNumber(pie.number - 1)
            nextPie = getPieByNumber(pie.number + 1)
            if(prevPie != null && prevPie.data == null) {
                $.ajax({async: true,
                    type: 'GET',
                    url: '/book/' + options.bookId + '/' + String(prevPie.number) + ".pie",
                    success: function(data) {
                        prevPie.data = data;
                    }});
            }
            if(nextPie != null && nextPie.data == null) {
                $.ajax({async: true,
                    type: 'GET',
                    url: '/book/' + options.bookId + '/' + String(nextPie.number) + ".pie",
                    success: function(data) {
                        nextPie.data = data;
                    }});
            }
        }

        /**
         * Найти pie которая отображает заданную позицию книги.
         * @param position позиция книги от 0 до 1.
         * @returns найденный content page.
         */
        function findPie(position) {
            var resultPie = null;
            $.each(pies, function(index, pageIterator){
                if(position >= pageIterator.pieMin && position <= pageIterator.pieMax){
                    resultPie = pageIterator;
                }
            });
            return resultPie;
        }

        /**
         * Найти pie которая содержит элемент с заданным id.
         * @returns найденный content page.
         */
        function findPieById(id) {
            var resultPie = null;
            $.each(pies, function(index, pieIterator){
                var elem = jQuery.inArray(id, pieIterator.ids)
                if(elem > -1) {
                    resultPie = pieIterator
                    return false;
                }
            });
            return resultPie;
        }
    }
})(jQuery);