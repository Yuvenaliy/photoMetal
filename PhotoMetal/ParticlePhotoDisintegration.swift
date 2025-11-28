//
//  ParticlePhotoDisintegration.swift
//  PhotoMetal
//
//  One-file module: configuration, Metal shader, renderer, controller, and debug overlay.
//

import UIKit
import MetalKit
import simd
import OSLog
import SwiftUI

// MARK: - Logging

enum Log {
    private static let logger = Logger(subsystem: "ParticlePhotoDisintegration", category: "Metal")

    static func info(_ message: String) {
        logger.info("\(message, privacy: .public)")
    }

    static func error(_ message: String) {
        logger.error("\(message, privacy: .public)")
    }
}

// MARK: - Config & shared parameters

enum ParticleEffectConfig {
    /// Pixel sampling stride (3 -> each 3rd pixel in X/Y).
    static let samplingStride: Int = 3

    /// Hard particle cap to avoid runaway allocations.
    static let maxParticles: Int = 400_000

    /// Animation duration for explode/implode.
    static let animationDuration: Float = 2.0

    /// Shared defaults for the brand look.
    static let defaultParameters = ParticleEffectParameters(
        noiseScale: 3.0,
        explosionStrength: 2.5,
        radialStrength: 0.65,
        curlStrength: 1.25,
        colorInfluence: 0.35,
        pointSize: 2.0
    )
}

/// Live-tweakable parameters that define the "brand" look of the effect.
final class ParticleEffectParameters: ObservableObject {
    @Published var noiseScale: Float
    @Published var explosionStrength: Float
    @Published var radialStrength: Float
    @Published var curlStrength: Float
    @Published var colorInfluence: Float
    @Published var pointSize: Float

    init(noiseScale: Float,
         explosionStrength: Float,
         radialStrength: Float,
         curlStrength: Float,
         colorInfluence: Float,
         pointSize: Float) {
        self.noiseScale = noiseScale
        self.explosionStrength = explosionStrength
        self.radialStrength = radialStrength
        self.curlStrength = curlStrength
        self.colorInfluence = colorInfluence
        self.pointSize = pointSize
    }

    convenience init(copying other: ParticleEffectParameters = ParticleEffectConfig.defaultParameters) {
        self.init(noiseScale: other.noiseScale,
                  explosionStrength: other.explosionStrength,
                  radialStrength: other.radialStrength,
                  curlStrength: other.curlStrength,
                  colorInfluence: other.colorInfluence,
                  pointSize: other.pointSize)
    }
}

/// Shared "brand" parameters to reuse the same look across screens.
enum ParticleBranding {
    static let sharedParameters = ParticleEffectParameters(copying: ParticleEffectConfig.defaultParameters)
}

// MARK: - GPU models (Swift ↔ Metal)

struct GpuParticle {
    var position: SIMD2<Float> // View-space position in pixels
    var uv: SIMD2<Float>       // Normalized [0,1] UV
}

struct Uniforms {
    var viewportSize: SIMD2<Float>
    var time: Float
    var progress: Float
    var pointSize: Float
    var noiseScale: Float
    var curlStrength: Float
    var radialStrength: Float
    var explosionStrength: Float
    var colorInfluence: Float
}

// MARK: - Shader source with curl-noise

enum ShaderSource {
    static let metal = """
    #include <metal_stdlib>
    using namespace metal;

    struct Particle {
        float2 position;
        float2 uv;
    };

    struct Uniforms {
        float2 viewportSize;
        float time;
        float progress;
        float pointSize;
        float noiseScale;
        float curlStrength;
        float radialStrength;
        float explosionStrength;
        float colorInfluence;
    };

    struct VSOut {
        float4 position [[position]];
        float2 uv;
        float pointSize [[point_size]];
        float alpha;
    };

    constexpr sampler imageSampler(address::clamp_to_edge, filter::linear);

    // --- Simplex Noise Primitives (compact) ---
    float3 mod289(float3 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
    float2 mod289(float2 x) { return x - floor(x * (1.0 / 289.0)) * 289.0; }
    float3 permute(float3 x) { return mod289(((x*34.0)+1.0)*x); }

    float snoise(float2 v) {
        const float4 C = float4(0.211324865405187, 0.366025403784439,
                                -0.577350269189626, 0.024390243902439);
        float2 i  = floor(v + dot(v, C.yy));
        float2 x0 = v - i + dot(i, C.xx);

        float2 i1 = (x0.x > x0.y) ? float2(1.0, 0.0) : float2(0.0, 1.0);
        float4 x12 = x0.xyxy + C.xxzz;
        x12.xy -= i1;

        i = mod289(i);
        float3 p = permute(permute(
                            i.y + float3(0.0, i1.y, 1.0))
                            + i.x + float3(0.0, i1.x, 1.0));

        float3 m = max(0.5 - float3(dot(x0,x0),
                                    dot(x12.xy,x12.xy),
                                    dot(x12.zw,x12.zw)), 0.0);
        m = m*m;
        m = m*m;

        float3 x = 2.0 * fract(p * C.www) - 1.0;
        float3 h = abs(x) - 0.5;
        float3 ox = floor(x + 0.5);
        float3 a0 = x - ox;

        m *= 1.79284291400159 - 0.85373472095314 * (a0*a0 + h*h);

        float3 g;
        g.x  = a0.x  * x0.x  + h.x  * x0.y;
        g.yz = a0.yz * x12.xz + h.yz * x12.yw;

        return 130.0 * dot(m, g);
    }

    // --- Curl Noise ---
    float2 curl(float2 p, float z) {
        float eps = 0.1;

        float2 ex = float2(eps, 0.0);
        float2 ey = float2(0.0, eps);

        float n1 = snoise(p + ex + z);
        float n2 = snoise(p - ex + z);
        float n3 = snoise(p + ey + z);
        float n4 = snoise(p - ey + z);

        float dsdx = (n3 - n4) / (2.0 * eps);
        float dsdy = (n1 - n2) / (2.0 * eps);

        return float2(dsdx, -dsdy);
    }

    vertex VSOut particleVertex(
        uint vertexId [[vertex_id]],
        constant Particle *particles [[buffer(0)]],
        constant Uniforms &u [[buffer(1)]],
        texture2d<float> imageTexture [[texture(0)]]
    ) {
        Particle p = particles[vertexId];

        float2 base = float2(
            (p.position.x / u.viewportSize.x) * 2.0 - 1.0,
            1.0 - (p.position.y / u.viewportSize.y) * 2.0
        );

        float t = clamp(u.progress, 0.0, 1.0);
        float eased = smoothstep(0.0, 1.0, t);

        float2 rel = base;
        float dist = length(rel);
        float2 dir = (dist > 1e-5) ? rel / dist : float2(0.0, 0.0);

        float2 noisePos = base * u.noiseScale;
        float z = t * 4.0;
        float2 curlVelocity = curl(noisePos, z) * u.curlStrength;

        float distFactor = 0.3 + dist;
        float2 radialOffset = dir * u.radialStrength * distFactor;

        float3 colorSample = imageTexture.sample(imageSampler, p.uv).rgb;
        float luminance = dot(colorSample, float3(0.2126, 0.7152, 0.0722));
        float colorFactor = mix(1.0, clamp(luminance, 0.05, 1.0), u.colorInfluence);

        float2 offset = (radialOffset + curlVelocity) * colorFactor;
        offset *= eased * u.explosionStrength;

        float2 finalPos = base + offset;

        VSOut out;
        out.position = float4(finalPos, 0.0, 1.0);
        out.uv = p.uv;
        out.pointSize = u.pointSize * (1.0 + 2.0 * eased);
        out.alpha = mix(1.0, 0.75, t) * (0.7 + 0.3 * colorFactor);
        return out;
    }

    fragment float4 particleFragment(
        VSOut in [[stage_in]],
        texture2d<float> imageTexture [[texture(0)]]
    ) {
        float4 color = imageTexture.sample(imageSampler, in.uv);
        if (color.a < 0.05) {
            discard_fragment();
        }
        return color * in.alpha;
    }
    """
}

// MARK: - Renderer

final class ParticlePhotoRenderer: NSObject, MTKViewDelegate {

    enum AnimationDirection {
        case idle
        case forward   // explode: 0 → 1
        case backward  // implode: 1 → 0
    }

    // Core Metal
    private let device: MTLDevice
    private unowned let mtkView: MTKView
    private let commandQueue: MTLCommandQueue
    private var pipelineState: MTLRenderPipelineState!

    // Resources
    private var vertexBuffer: MTLBuffer?
    private var particleCount: Int = 0
    private let textureLoader: MTKTextureLoader
    private var texture: MTLTexture?
    private var sourceImage: UIImage?

    // Animation state
    private var startTime: CFTimeInterval = CACurrentMediaTime()
    private var animationDirection: AnimationDirection = .idle
    private var animationStartTime: CFTimeInterval?
    private var animationBaseProgress: Float = 0.0
    private(set) var progress: Float = 0.0

    // Parameters
    private let parameters: ParticleEffectParameters
    private var needsParticleRebuild: Bool = false

    // MARK: - Init

    init(device: MTLDevice, mtkView: MTKView, parameters: ParticleEffectParameters = ParticleBranding.sharedParameters) {
        self.device = device
        self.mtkView = mtkView
        self.commandQueue = device.makeCommandQueue()!
        self.textureLoader = MTKTextureLoader(device: device)
        self.parameters = parameters
        super.init()
        mtkView.delegate = self
        buildPipeline()
    }

    // MARK: - Public API

    func setImage(_ image: UIImage) {
        self.sourceImage = image
        self.texture = makeTexture(from: image)
        progress = 0.0
        animationDirection = .idle
        animationStartTime = nil
        animationBaseProgress = 0.0
        needsParticleRebuild = true
        Log.info("Set new image, created texture, scheduled particle rebuild.")
    }

    func startExplode() {
        guard particleCount > 0 else { return }
        let now = CACurrentMediaTime()
        animationDirection = .forward
        animationBaseProgress = progress
        animationStartTime = now
        Log.info("Start explode from progress=\(self.progress)")
    }

    func startImplode() {
        guard particleCount > 0 else { return }
        let now = CACurrentMediaTime()
        animationDirection = .backward
        animationBaseProgress = progress
        animationStartTime = now
        Log.info("Start implode from progress=\(self.progress)")
    }

    func toggleDirection() {
        switch animationDirection {
        case .idle:
            if progress < 0.5 {
                startExplode()
            } else {
                startImplode()
            }
        case .forward:
            startImplode()
        case .backward:
            startExplode()
        }
    }

    // MARK: - Pipeline

    private func buildPipeline() {
        do {
            let library = try device.makeLibrary(source: ShaderSource.metal, options: nil)
            let vertexFunction = library.makeFunction(name: "particleVertex")
            let fragmentFunction = library.makeFunction(name: "particleFragment")

            let descriptor = MTLRenderPipelineDescriptor()
            descriptor.vertexFunction = vertexFunction
            descriptor.fragmentFunction = fragmentFunction
            descriptor.colorAttachments[0].pixelFormat = mtkView.colorPixelFormat

            pipelineState = try device.makeRenderPipelineState(descriptor: descriptor)
            Log.info("Render pipeline created")
        } catch {
            fatalError("Failed to create pipeline state: \(error)")
        }
    }

    private func makeTexture(from image: UIImage) -> MTLTexture? {
        guard let cgImage = image.cgImage else {
            Log.error("UIImage has no CGImage backing")
            return nil
        }

        do {
            let texture = try textureLoader.newTexture(cgImage: cgImage, options: [
                MTKTextureLoader.Option.SRGB : false
            ])
            Log.info("Texture created: \(texture.width)x\(texture.height)")
            return texture
        } catch {
            Log.error("Failed to create texture: \(String(describing: error))")
            return nil
        }
    }

    // MARK: - MTKViewDelegate

    func mtkView(_ view: MTKView, drawableSizeWillChange size: CGSize) {
        needsParticleRebuild = true
        Log.info("Drawable size will change: \(size.width)x\(size.height)")
    }

    func draw(in view: MTKView) {
        guard let texture = texture else { return }

        if needsParticleRebuild {
            rebuildParticles(for: view.drawableSize)
            needsParticleRebuild = false
        }

        guard particleCount > 0 else { return }
        guard let drawable = view.currentDrawable,
              let renderPassDescriptor = view.currentRenderPassDescriptor
        else { return }

        let now = CACurrentMediaTime()
        let elapsed = Float(now - startTime)

        updateAnimation(currentTime: now)

        var uniforms = Uniforms(
            viewportSize: SIMD2(Float(view.drawableSize.width),
                                Float(view.drawableSize.height)),
            time: elapsed,
            progress: progress,
            pointSize: parameters.pointSize,
            noiseScale: parameters.noiseScale,
            curlStrength: parameters.curlStrength,
            radialStrength: parameters.radialStrength,
            explosionStrength: parameters.explosionStrength,
            colorInfluence: parameters.colorInfluence
        )

        guard let commandBuffer = commandQueue.makeCommandBuffer() else { return }
        guard let encoder = commandBuffer.makeRenderCommandEncoder(descriptor: renderPassDescriptor) else { return }

        encoder.setRenderPipelineState(pipelineState)

        if let vertexBuffer = vertexBuffer {
            encoder.setVertexBuffer(vertexBuffer, offset: 0, index: 0)
        }

        encoder.setVertexBytes(&uniforms,
                               length: MemoryLayout<Uniforms>.stride,
                               index: 1)

        encoder.setVertexTexture(texture, index: 0)
        encoder.setFragmentTexture(texture, index: 0)

        encoder.drawPrimitives(type: .point,
                               vertexStart: 0,
                               vertexCount: particleCount)

        encoder.endEncoding()
        commandBuffer.present(drawable)
        commandBuffer.commit()
    }

    // MARK: - Animation

    private func updateAnimation(currentTime now: CFTimeInterval) {
        guard let start = animationStartTime,
              animationDirection != .idle
        else { return }

        let elapsed = Float(now - start)
        let duration = ParticleEffectConfig.animationDuration

        let delta = elapsed / duration

        switch animationDirection {
        case .forward:
            progress = min(animationBaseProgress + delta, 1.0)
            if progress >= 1.0 {
                animationDirection = .idle
                animationStartTime = nil
                animationBaseProgress = 1.0
                Log.info("Explode finished")
            }
        case .backward:
            progress = max(animationBaseProgress - delta, 0.0)
            if progress <= 0.0 {
                animationDirection = .idle
                animationStartTime = nil
                animationBaseProgress = 0.0
                Log.info("Implode finished")
            }
        case .idle:
            break
        }
    }

    // MARK: - Particles generation

    private func rebuildParticles(for drawableSize: CGSize) {
        guard let image = sourceImage,
              let cgImage = image.cgImage
        else {
            Log.error("No image to build particles from")
            particleCount = 0
            vertexBuffer = nil
            return
        }

        let imgWidth = cgImage.width
        let imgHeight = cgImage.height
        guard imgWidth > 0, imgHeight > 0 else {
            Log.error("Invalid image size")
            return
        }

        let viewWidth = drawableSize.width
        let viewHeight = drawableSize.height
        guard viewWidth > 0, viewHeight > 0 else {
            Log.error("Drawable size is zero")
            return
        }

        let imageAspect = CGFloat(imgWidth) / CGFloat(imgHeight)
        let viewAspect = viewWidth / viewHeight

        let scaledWidth: CGFloat
        let scaledHeight: CGFloat

        if imageAspect > viewAspect {
            scaledWidth = viewWidth
            scaledHeight = viewWidth / imageAspect
        } else {
            scaledHeight = viewHeight
            scaledWidth = viewHeight * imageAspect
        }

        let originX = (viewWidth - scaledWidth) * 0.5
        let originY = (viewHeight - scaledHeight) * 0.5

        let stride = max(1, ParticleEffectConfig.samplingStride)
        let maxParticles = ParticleEffectConfig.maxParticles

        var particles: [GpuParticle] = []
        particles.reserveCapacity(min(maxParticles,
                                      (imgWidth / stride) * (imgHeight / stride)))

        var count = 0

        for y in Swift.stride(from: 0, to: imgHeight, by: stride) {
            for x in Swift.stride(from: 0, to: imgWidth, by: stride) {
                if count >= maxParticles { break }

                let fx = CGFloat(x) + 0.5
                let fy = CGFloat(y) + 0.5

                let imageXNorm = fx / CGFloat(imgWidth)
                let imageYNorm = fy / CGFloat(imgHeight)

                let posX = originX + imageXNorm * scaledWidth
                let posY = originY + imageYNorm * scaledHeight

                let position = SIMD2(Float(posX), Float(posY))
                let uv = SIMD2(Float(imageXNorm), Float(imageYNorm))

                particles.append(GpuParticle(position: position, uv: uv))
                count += 1
            }
            if count >= maxParticles { break }
        }

        let length = particles.count * MemoryLayout<GpuParticle>.stride
        vertexBuffer = device.makeBuffer(bytes: particles,
                                         length: length,
                                         options: [.storageModeShared])
        particleCount = particles.count
        Log.info("Particles rebuilt: \(self.particleCount)")
    }
}

// MARK: - Debug panel (SwiftUI overlay)

@available(iOS 13.0, *)
struct ParticleDebugPanel: View {
    @ObservedObject var parameters: ParticleEffectParameters
    @State private var isExpanded = false

    var body: some View {
        VStack(alignment: .leading, spacing: 8) {
            HStack {
                Text("Curl Noise")
                    .font(.headline)
                Spacer()
                Button(action: { isExpanded.toggle() }) {
                    Image(systemName: isExpanded ? "chevron.down" : "chevron.right")
                        .font(.headline)
                }
                .buttonStyle(.plain)
            }

            if isExpanded {
                sliderRow(title: "Noise scale",
                          value: Binding(get: { parameters.noiseScale },
                                         set: { parameters.noiseScale = $0 }),
                          range: 0.5...8.0,
                          format: "%.2f")
                sliderRow(title: "Curl amplitude",
                          value: Binding(get: { parameters.curlStrength },
                                         set: { parameters.curlStrength = $0 }),
                          range: 0.2...3.0,
                          format: "%.2f")
                sliderRow(title: "Radial push",
                          value: Binding(get: { parameters.radialStrength },
                                         set: { parameters.radialStrength = $0 }),
                          range: 0.1...2.5,
                          format: "%.2f")
                sliderRow(title: "Explosion",
                          value: Binding(get: { parameters.explosionStrength },
                                         set: { parameters.explosionStrength = $0 }),
                          range: 0.5...4.0,
                          format: "%.2f")
                sliderRow(title: "Color influence",
                          value: Binding(get: { parameters.colorInfluence },
                                         set: { parameters.colorInfluence = $0 }),
                          range: 0.0...1.0,
                          format: "%.2f")
                sliderRow(title: "Point size",
                          value: Binding(get: { parameters.pointSize },
                                         set: { parameters.pointSize = $0 }),
                          range: 1.0...6.0,
                          format: "%.1f")
            }
        }
        .padding(12)
        .background(
            RoundedRectangle(cornerRadius: 14, style: .continuous)
                .fill(Color.black.opacity(0.55))
        )
        .overlay(
            RoundedRectangle(cornerRadius: 14, style: .continuous)
                .stroke(Color.white.opacity(0.2), lineWidth: 1)
        )
        .foregroundColor(.white)
    }

    private func sliderRow(title: String,
                           value: Binding<Float>,
                           range: ClosedRange<Float>,
                           format: String) -> some View {
        VStack(alignment: .leading, spacing: 4) {
            HStack {
                Text(title)
                Spacer()
                Text(String(format: format, value.wrappedValue))
                    .font(.caption.monospacedDigit())
                    .foregroundColor(.white.opacity(0.8))
            }
            Slider(value: value.mapToDouble(), in: Double(range.lowerBound)...Double(range.upperBound))
        }
    }
}

private extension Binding where Value == Float {
    func mapToDouble() -> Binding<Double> {
        Binding<Double>(
            get: { Double(self.wrappedValue) },
            set: { self.wrappedValue = Float($0) }
        )
    }
}

// MARK: - UIViewController wrapper

final class ParticlePhotoDisintegrationViewController: UIViewController {

    private var mtkView: MTKView!
    private var renderer: ParticlePhotoRenderer!
    private let parameters: ParticleEffectParameters
    private let showDebugOverlay: Bool

    init(parameters: ParticleEffectParameters = ParticleBranding.sharedParameters,
         showDebugOverlay: Bool = true) {
        self.parameters = parameters
        self.showDebugOverlay = showDebugOverlay
        super.init(nibName: nil, bundle: nil)
    }

    required init?(coder: NSCoder) {
        fatalError("init(coder:) has not been implemented")
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        view.backgroundColor = .black

        guard let device = MTLCreateSystemDefaultDevice() else {
            fatalError("Metal is not supported on this device")
        }

        let metalView = MTKView(frame: view.bounds, device: device)
        metalView.autoresizingMask = [.flexibleWidth, .flexibleHeight]
        metalView.clearColor = MTLClearColorMake(0.0, 0.0, 0.0, 1.0)
        metalView.colorPixelFormat = .bgra8Unorm
        metalView.preferredFramesPerSecond = 60
        metalView.enableSetNeedsDisplay = false
        metalView.isPaused = false

        view.addSubview(metalView)

        self.mtkView = metalView
        self.renderer = ParticlePhotoRenderer(device: device, mtkView: metalView, parameters: parameters)

        if let image = UIImage(named: "example_photo") {
            renderer.setImage(image)
        } else if let fallback = Self.makeGradientFallback() {
            renderer.setImage(fallback)
        } else if let systemImage = UIImage(systemName: "person.fill") {
            renderer.setImage(systemImage)
        } else {
            Log.error("Failed to load any demo image")
        }

        if showDebugOverlay {
            attachDebugOverlay()
        }

        Log.info("Tap anywhere to toggle explode/implode")
    }

    // Toggle explode/implode on tap
    override func touchesEnded(_ touches: Set<UITouch>, with event: UIEvent?) {
        renderer.toggleDirection()
    }

    private func attachDebugOverlay() {
        guard #available(iOS 13.0, *) else { return }
        let panel = ParticleDebugPanel(parameters: parameters)
        let host = UIHostingController(rootView: panel)
        host.view.backgroundColor = .clear
        addChild(host)
        view.addSubview(host.view)
        host.didMove(toParent: self)
        host.view.translatesAutoresizingMaskIntoConstraints = false

        NSLayoutConstraint.activate([
            host.view.leadingAnchor.constraint(equalTo: view.safeAreaLayoutGuide.leadingAnchor, constant: 12),
            host.view.trailingAnchor.constraint(lessThanOrEqualTo: view.safeAreaLayoutGuide.trailingAnchor, constant: -12),
            host.view.bottomAnchor.constraint(equalTo: view.safeAreaLayoutGuide.bottomAnchor, constant: -12)
        ])
    }

    private static func makeGradientFallback() -> UIImage? {
        let size = CGSize(width: 600, height: 800)
        let renderer = UIGraphicsImageRenderer(size: size)
        return renderer.image { ctx in
            let colors = [UIColor.systemPink.cgColor,
                          UIColor.systemOrange.cgColor,
                          UIColor.systemTeal.cgColor]
            guard let gradient = CGGradient(colorsSpace: CGColorSpaceCreateDeviceRGB(),
                                            colors: colors as CFArray,
                                            locations: [0.0, 0.5, 1.0]) else { return }
            ctx.cgContext.drawLinearGradient(gradient,
                                             start: CGPoint(x: 0, y: 0),
                                             end: CGPoint(x: size.width, y: size.height),
                                             options: [])
            let circleRect = CGRect(x: size.width * 0.25,
                                    y: size.height * 0.25,
                                    width: size.width * 0.5,
                                    height: size.width * 0.5)
            ctx.cgContext.setFillColor(UIColor.white.withAlphaComponent(0.9).cgColor)
            ctx.cgContext.fillEllipse(in: circleRect)
            ctx.cgContext.setFillColor(UIColor.black.withAlphaComponent(0.8).cgColor)
            let font = UIFont.boldSystemFont(ofSize: 120)
            let text = "PM" as NSString
            let textSize = text.size(withAttributes: [.font: font])
            let textOrigin = CGPoint(x: (size.width - textSize.width) * 0.5,
                                     y: (size.height - textSize.height) * 0.5)
            text.draw(at: textOrigin, withAttributes: [.font: font,
                                                       .foregroundColor: UIColor.black])
        }
    }
}
