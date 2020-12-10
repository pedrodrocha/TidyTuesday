async function drawTrails(){
    // 1. LOAD DATA
    const dataset = await d3.csv('hike_trails.csv')

    const dataset_sorted = dataset.sort(function(a,b){
            return a["mean_highpoint_m"]-b["mean_highpoint_m"];
        });
    dataset_sorted.forEach((d,i) => {
        d['key'] = i + 1
    });

        // 1.1 Build accessors
            //Y accessor will be Highestpoint above sea level
    const yAccessor = d => d.mean_highpoint_m
            // X accessor will be region
    const xAccessor = d => d.key

    // 2. CREATE DIMENSIONS 
    let dimensions = {
        width: 700,
        height: 800,
        margin: {
            // We'll leave a larger margin on top to account for the bar labels
            top: 140,
            right: 90,
            bottom: 0, 
            left: 50,
        },
    };

    dimensions.boundedWidth = dimensions.width
        - dimensions.margin.left
        - dimensions.margin.right
    dimensions.boundedHeight = dimensions.height
        - dimensions.margin.top
        - dimensions.margin.bottom

    // 3. DRAW CANVAS
    const wrapper = d3.select('#wrapper')
        .append('svg')
            .attr('width', dimensions.width)
            .attr('height', dimensions.height)
    const bounds = wrapper
        .append('g')
            .style(
                'transform',
                `translate(${dimensions.margin.left}px,
                        ${dimensions.margin.top}px)`
    );

    // 4. CREATE SCALES
    const xScale = d3.scaleLinear()
        .domain(d3.extent(dataset, xAccessor))
        .range([0, dimensions.boundedWidth])
    const yScale = d3.scaleLinear()
        .domain([1500,18000])
        .range([dimensions.boundedWidth,0])


    // 5. DRAW DATA
    const lineGenerator = d3.line()
        .x(d => xScale(xAccessor(d)))
        .y(d => yScale(yAccessor(d)))

    const line = bounds.append('path')
        .attr('d',lineGenerator(dataset_sorted))
        .attr('class','line')

    const dots = bounds.selectAll('circle')
        .data(dataset_sorted)
        .enter().append('circle')
        .attr('class','dots')
        .attr('cx', d => xScale(xAccessor(d)))
        .attr('cy', d => yScale(yAccessor(d)))
        .attr('r',4)

    const text = bounds.selectAll('text')
        .data(dataset_sorted)
        .enter().append('text')
        .attr('id', d => `label${d.key}`)
        .attr('class','label')
        .attr('x', d => xScale(xAccessor(d)) + 10)
        .attr('y', d => yScale(yAccessor(d)) + 10)
        .text(d => d.region)

    // Adjusting labels

    d3.select('#label6')
        .style('transform','translate(-190px,0px)')

    d3.select('#label8')
        .style('transform','translate(-150px,0px)')
    
    d3.select('#label10')
        .style('transform','translate(-150px,0px)')
    d3.select('#label11')
        .style('transform','translate(-170px,-20px)')

    // 6. DRAW PERIPHERALS (para ir para detrás)

    // Y Axis
    const yAxisGenerator = d3.axisRight()
        .scale(yScale)
        .tickValues([1500,3500,5500,7500,9500,11500,13500,15500,17500])
        .tickSize(-dimensions.boundedWidth - 60)

    const yAxis = bounds.append('g')
        .call(yAxisGenerator)
            .attr("transform", "translate(600,0)")
            .attr('class','y-axis')

    d3.select(".line")
        .raise()
    d3.selectAll('.dots')
        .raise()
    d3.selectAll('.label')
        .raise()

    const Title = wrapper.append('text')
        .attr('x',449)
        .attr("text-anchor", "end")  
        .attr('y', 55)
        .attr('class','title')
        .text('Washington Trails')

    const subtitle = wrapper.append('text')
        .attr('x',700)
        .attr("text-anchor", "end")  
        .attr('y', 95)
        .attr('class','subtitle')
        .text('Avg. highest point (m) above sea level of trails per Washington region')

    const caption = wrapper.append('text')
        .attr('x',485)
        .attr("text-anchor", "end")  
        .attr('y', 750)
        .attr('class','caption')
        .text('Tidy Tuesday Week 48 | Viz: @pedro_drocha | Source: TidyX Crew (Ellis Hughes & Patrick Ward)')

    // 7. INTERACTIONS
    // Draw Delaynay for better interaction with the tooltip

    bounds.selectAll('.label')
            .on('mouseenter',onMouseEnter)
            .on('mouseleave',onMouseLeave)

    const tooltip = d3.select('#tooltip')

    function onMouseEnter(event,datum){
        const index = d3.selectAll('circle').nodes().indexOf(this)

        const formatRating = d3.format(".2f")
        tooltip
            .select('#rating')
            .text(formatRating(datum.mean_rating))

        tooltip
            .select('#features')
            .text(datum.features)

        // Positioning the tooltip


        const x = event.x
        const y = event.y
        console.log(y)
        tooltip
            .style("transform",
                    `translate(` 
                    + `calc( -50% + ${x}px),`
                    + `calc(-100% + ${y}px)`
                    + `)`)
        

        tooltip
        .transition().duration(500)
        .style('opacity',1)
    }

    function onMouseLeave(event,datum){
        tooltip
        .transition().duration(500)
        .style('opacity',0)
    }
};

drawTrails();